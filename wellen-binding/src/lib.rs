use std::{ffi::*, ops::Deref, panic, ptr};
use wellen::{Hierarchy, SignalRef, SignalValue, Var, VarRef, simple::*};

#[unsafe(no_mangle)]
pub extern "C" fn load_file(c_str: *const c_char) -> *mut c_void {
    unsafe {
        let c_str = CStr::from_ptr(c_str);
        match c_str.to_str() {
            Ok(path) => {
                let result = panic::catch_unwind(|| match read(path) {
                    Result::Ok(waveform) => Result::Ok(waveform),
                    Result::Err(err) => Result::Err(err),
                });
                match result {
                    Result::Ok(maybe_waveform) => match maybe_waveform {
                        Result::Ok(waveform) => {
                            return Box::into_raw(Box::new(waveform)) as *mut c_void;
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
pub extern "C" fn free_waveform(waveform_ptr: *mut c_void) {
    println!("Dropping Waveform");
    unsafe {
        drop(Box::from_raw(waveform_ptr));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn load_vars(waveform_ptr: *mut c_void, vars_ptr: *const usize, count: usize) {
    unsafe {
        let waveform: &mut Waveform = &mut *(waveform_ptr.cast());
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
pub extern "C" fn unload_vars(waveform_ptr: *mut c_void, vars_ptr: *const usize, count: usize) {
    unsafe {
        let waveform: &mut Waveform = &mut *(waveform_ptr.cast());
        let hierarchy = waveform.hierarchy();

        assert_eq!(vars_ptr as usize % std::mem::align_of::<usize>(), 0);
        assert!(!vars_ptr.is_null());

        print!("Rust unloading: ");
        let vars = std::slice::from_raw_parts(vars_ptr, count);
        println!("{:?}", vars);

        for x in vars {
            println!("{:?}", *x);
        }
        println!("loop");

        // let x = hierarchy.vars().next();

        let vec: Vec<Var> = vars
            .iter()
            .filter_map(|i| {
                let x = *i;
                println!("FM: {x}");
                println!("FM2: {:?}", VarRef::from_index(x));
                VarRef::from_index(x)
            })
            .map(|var_ref| hierarchy[var_ref].clone())
            .collect();

        // waveform.unload_signals(vec.as_slice());
    }
}

#[repr(C)]
pub struct CSlice<S> {
    ptr: *const S,
    length: usize,
}

// #[unsafe(no_mangle)]
// pub extern "C" fn get_all_signals(waveform_ptr: *mut c_void) -> CSlice<CString> {
//     unsafe {
//         let waveform: &mut Waveform = &mut *(waveform_ptr.cast());
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
pub extern "C" fn lookup_var(waveform_ptr: *mut c_void, name_ptr: *const c_char) -> isize {
    unsafe {
        let waveform: &mut Waveform = &mut *(waveform_ptr.cast());
        let hierarchy = waveform.hierarchy();
        let c_str = CStr::from_ptr(name_ptr);

        match c_str.to_str() {
            Ok(signal_name) => {
                let parts: Vec<&str> = signal_name.split('.').collect();
                let name = parts[parts.len() - 1];
                let path = &parts[0..parts.len() - 1];
                let found = hierarchy.lookup_var(path, &name);
                match found {
                    Some(var_ref) => {
                        return var_ref.index() as isize;
                    }
                    None => return -1,
                }
            }
            Err(err) => {
                eprintln!("Invalid signal name: {err}");
                return -1;
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn get_times(waveform_ptr: *mut c_void) -> *const CSlice<u64> {
    unsafe {
        let waveform: &mut Waveform = &mut *(waveform_ptr.cast());
        let times = waveform.time_table();
        let c_slice = CSlice {
            ptr: times.as_ptr(),
            length: times.len(),
        };
        return Box::into_raw(Box::new(c_slice));
    }
}

#[repr(C)]
pub struct SignalResult {
    signal_type: isize,
    word: u64,
    bytes: *const u8,
    length: usize,
}

#[unsafe(no_mangle)]
pub extern "C" fn get_var(
    waveform_ptr: *mut c_void,
    var_index: usize,
    time_index: u32,
) -> *const SignalResult {
    unsafe {
        let waveform: &mut Waveform = &mut *(waveform_ptr.cast());
        let hierarchy = waveform.hierarchy();
        let var_ref = VarRef::from_index(var_index).unwrap();
        let signal = waveform
            .get_signal(hierarchy[var_ref].signal_ref())
            .unwrap();

        let maybe_offset = signal.get_offset(time_index);
        return Box::into_raw(Box::new(match maybe_offset {
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
        }));
    }
}
