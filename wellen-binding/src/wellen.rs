use std::{collections::HashMap, ffi::*, ops::Deref, panic, ptr};
use wellen::{
    Hierarchy, LoadOptions, SignalRef, SignalValue, Timescale, TimescaleUnit, Var, VarRef,
    simple::*,
};

#[repr(C)]
pub struct CSlice<S> {
    ptr: *const S,
    length: usize,
}

#[repr(C)]
pub struct SignalResult {
    signal_type: isize,
    word: u64,
    bytes: *const u8,
    length: usize,
}

#[repr(C)]
pub struct Manager {
    waveform: Waveform,
    name_cache: HashMap<String, VarRef>,
}

impl<'a> Manager {
    fn into_pointer(self) -> *mut c_void {
        return Box::into_raw(Box::new(self)) as *mut c_void;
    }

    unsafe fn from_pointer(ptr: *mut c_void) -> &'a mut Self {
        return &mut *(ptr.cast());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn load_file(c_str: *const c_char) -> *mut c_void {
    unsafe {
        let c_str = CStr::from_ptr(c_str);
        match c_str.to_str() {
            Ok(path) => {
                let result = panic::catch_unwind(|| {
                    match read_with_options(
                        path,
                        &LoadOptions {
                            multi_thread: true,
                            remove_scopes_with_empty_name: true,
                        },
                    ) {
                        Result::Ok(waveform) => Result::Ok(waveform),
                        Result::Err(err) => Result::Err(err),
                    }
                });
                match result {
                    Result::Ok(maybe_waveform) => match maybe_waveform {
                        Result::Ok(waveform) => {
                            let manager = Manager {
                                waveform,
                                name_cache: HashMap::new(),
                            };
                            return manager.into_pointer();
                        }
                        Result::Err(err) => {
                            eprintln!("Loading error: {err}");
                        }
                    },
                    Result::Err(_) => {
                        eprintln!("Panic during file loading");
                    }
                }
            }
            Err(_) => eprintln!("Received invalid string"),
        }
    }

    return ptr::null_mut();
}

#[unsafe(no_mangle)]
pub extern "C" fn free_waveform(manager_ptr: *mut c_void) {
    unsafe {
        drop(Box::from_raw(manager_ptr));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn load_vars(manager_ptr: *mut c_void, vars_ptr: *const usize, count: usize) {
    unsafe {
        let manager = Manager::from_pointer(manager_ptr);
        let waveform = &mut manager.waveform;
        let hierarchy = waveform.hierarchy();

        let vars = std::slice::from_raw_parts(vars_ptr, count);

        let vec: Vec<SignalRef> = vars
            .iter()
            .filter_map(|i| VarRef::from_index(*i))
            .map(|var_ref| hierarchy[var_ref].signal_ref())
            .collect();

        waveform.load_signals(vec.as_slice());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn unload_vars(manager_ptr: *mut c_void, vars_ptr: *const usize, count: usize) {
    unsafe {
        let manager = Manager::from_pointer(manager_ptr);
        let waveform = &mut manager.waveform;
        let hierarchy = waveform.hierarchy();

        assert_eq!(vars_ptr as usize % std::mem::align_of::<usize>(), 0);
        assert!(!vars_ptr.is_null());

        let vars = std::slice::from_raw_parts(vars_ptr, count);

        let vec: Vec<SignalRef> = vars
            .iter()
            .filter_map(|i| {
                let x = *i;
                VarRef::from_index(x)
            })
            .map(|var_ref| hierarchy[var_ref].signal_ref())
            .collect();

        waveform.unload_signals(vec.as_slice());
    }
}

// #[unsafe(no_mangle)]
// pub extern "C" fn get_all_signals(manager_ptr: *mut c_void) -> CSlice<CString> {
//     unsafe {
//         let waveform: &mut Waveform = &mut *(manager_ptr.cast());
//         let hierarchy = waveform.hierarchy();
//         let vars: Vec<String> = hierarchy
//             .iter_vars()
//             .map(|var| var.full_name(waveform.hierarchy()))
//             .collect();
//         return CSlice {
//             ptr: vars.as_slice(),
//             length: vars.len(),
//         };
//         panic!();
//     }
// }

#[unsafe(no_mangle)]
pub extern "C" fn lookup_var(manager_ptr: *mut c_void, name_ptr: *const u8, count: usize) -> isize {
    unsafe {
        let manager = Manager::from_pointer(manager_ptr);
        let waveform = &mut manager.waveform;
        let name_cache = &mut manager.name_cache;
        let hierarchy = waveform.hierarchy();
        let name_bytes = std::slice::from_raw_parts(name_ptr, count);
        let signal_name = str::from_utf8_unchecked(name_bytes);

        let cached_ref = name_cache.get(signal_name);

        match cached_ref {
            Some(cached) => return cached.index() as isize,
            None => {
                let parts: Vec<&str> = signal_name.split('.').collect();
                let name = parts[parts.len() - 1];
                let path = &parts[0..parts.len() - 1];
                let maybe_scope_ref = hierarchy.lookup_scope(path);
                match maybe_scope_ref {
                    Some(scope_ref) => {
                        let scope = &hierarchy[scope_ref];
                        let found = scope
                            .vars(hierarchy)
                            .find(|x| name == hierarchy[*x].name(hierarchy));

                        match found {
                            Some(var_ref) => {
                                for var_ref in scope.vars(hierarchy) {
                                    let var = &hierarchy[var_ref];
                                    name_cache.insert(var.full_name(hierarchy), var_ref);
                                }
                                return var_ref.index() as isize;
                            }
                            None => return -1,
                        }
                    }
                    None => return -1,
                }
            }
        }

        // let found = hierarchy.lookup_var(path, &name);
        // match found {
        //     Some(var_ref) => {
        //         return var_ref.index() as isize;
        //     }
        //     None => return -1,
        // }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn get_times(manager_ptr: *mut c_void) -> *const CSlice<u64> {
    unsafe {
        let manager = Manager::from_pointer(manager_ptr);
        let waveform = &mut manager.waveform;
        let times = waveform.time_table();
        let c_slice = CSlice {
            ptr: times.as_ptr(),
            length: times.len(),
        };
        return Box::into_raw(Box::new(c_slice));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn get_timescale(manager_ptr: *mut c_void) -> u64 {
    unsafe {
        let manager = Manager::from_pointer(manager_ptr);
        let waveform = &mut manager.waveform;
        match waveform.hierarchy().timescale() {
            None => return 0,
            Some(Timescale { factor, unit }) => {
                let unit_int = match unit {
                    TimescaleUnit::FemtoSeconds => 1,
                    TimescaleUnit::PicoSeconds => 2,
                    TimescaleUnit::NanoSeconds => 3,
                    TimescaleUnit::MicroSeconds => 4,
                    TimescaleUnit::MilliSeconds => 5,
                    TimescaleUnit::Seconds => 6,
                    TimescaleUnit::Unknown => 0,
                };
                ((factor as u64) << 32) | unit_int
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn get_var(
    manager_ptr: *mut c_void,
    var_index: usize,
    time_index: u32,
    result_ptr: *mut SignalResult,
) {
    unsafe {
        let manager = Manager::from_pointer(manager_ptr);
        let waveform = &mut manager.waveform;
        let hierarchy = waveform.hierarchy();
        let var_ref = VarRef::from_index(var_index).unwrap();
        let signal = waveform
            .get_signal(hierarchy[var_ref].signal_ref())
            .unwrap();

        let maybe_offset = signal.get_offset(time_index);
        *result_ptr = match maybe_offset {
            Some(offset) => {
                // TODO: Investiage the second parameter `element`
                let value = signal.get_value_at(&offset, 0);
                match value {
                    SignalValue::Binary(data, bits) => SignalResult {
                        signal_type: 0,
                        word: bits as u64,
                        bytes: data.as_ptr(),
                        length: data.len(),
                    },
                    SignalValue::FourValue(data, bits) => SignalResult {
                        signal_type: 1,
                        word: bits as u64,
                        bytes: data.as_ptr(),
                        length: data.len(),
                    },
                    SignalValue::NineValue(data, bits) => SignalResult {
                        signal_type: 2,
                        word: bits as u64,
                        bytes: data.as_ptr(),
                        length: data.len(),
                    },
                    SignalValue::String(data) => SignalResult {
                        signal_type: 3,
                        word: 0,
                        bytes: data.as_ptr(),
                        length: data.len(),
                    },
                    SignalValue::Real(f64) => SignalResult {
                        signal_type: 4,
                        word: f64.to_bits(),
                        bytes: ptr::null(),
                        length: 0,
                    },
                }
            }
            None => SignalResult {
                signal_type: -1,
                word: 0,
                bytes: ptr::null(),
                length: 0,
            },
        };
    }
}
