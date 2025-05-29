# Waveracer

Waveracer is a a Haskell library for analysing waveform files.

## Features

- **Load** fst/vcd/ghw files
- **Inspect** signal values at each point in time
- **Decode** signal values for further analysis

## Installation

Waveracer is a Haskell library and not a standalone tool. To install it, you will need:

- Haskell build tool `cabal`
- Haskell compiler `GHC>=9.10`
- Rust compiler
- Rust build tool `cargo`
- Rust tool `cbindgen`

### Build from Source

You can build from source:

```
git clone https://github.com/Simre1/waveracer.git
cd waveracer
sh build_rust.sh
cabal build
```

## Getting started

### Loading waveforms

You start using the library by loading waveforms with `loadWaveformFile` from a file path. 

```haskell
myWaveform :: IO Waveform
myWaveform = loadWaveformFile "my-waveform.fst"
```

### Simple Tracing

Here is an example of how to trace signals:
```haskell
runTrace :: Waveform -> Trace a -> IO a
runInspect :: Inspect a -> Trace [a]
load :: Text -> Trace Signal
inspect :: Signal -> Inspect SignalValue

example :: Waveform -> IO [SignalValue]
example wf = runTrace wf $ do
  mySignal <- load "mySignal"
  runInspect $ do
    myValue <- inspect mySignal 
    pure myValue
```

First, you need to open a tracing scope with `runTrace` and your loaded waveform. Within this scope, you can use `load` to load any signals via their name. Then, you use `runInspect` in combination with `inspect` to read those signal values. Your inspection code will run for each time point within the trace, that's why `example` returns a list of signal values.

### Scaling to many signals

`load` and `inspect` are just regular Haskell expressions. You can use everything from the Haskell toolbox to perform complex analysis.

```haskell
manySignals :: [Text]
manySignals = ["signal1", "signal2", ...]

example :: Waveform -> IO [[SignalValue]]
example wf = runTrace wf $ do
  signals <- traverse load manySignals
  runInspect $ do
    values <- traverse inspect signals
    pure values 
```

The above shows an example of loading a list of signals by using `traverse`.

### Decoding signal values

If you want to perform analysis on signal values, you first need to decode them. Usually, you will want to use either `decodeInt` or `decodeBitString`.

```haskell
decodeInt :: SignalValue -> Maybe Int
decodeBitString :: SignalValue -> Maybe Text

example :: Waveform -> IO [Maybe Int]
example wf = runTrace wf $ do
  mySignal <- load "mySignal"
  runInspect $ do
    myValue <- inspect mySignal 
    pure $ decodeInt myValue
```

The above example uses `decodeInt` to convert a `SignalValue` into an `Int`. However, this conversion can fail since `SignalValue`s can be partially undefined (e.g. don't cares, impedance, ...) or can even be simply text. This depends on your hardware implementation and the used simulator tool. 

### Sampling different timepoints

Usually, you do not want to sample all time points. You can modify the sampled time points with the functions `findIndices` and `sampleAt` or their combination `sampleOn`.

```haskell
findIndices :: Inspect Bool -> Trace [TimeIndex]
sampleAt :: [TimeIndex] -> Trace a -> Trace a
sampleOn :: Inspect Bool -> Trace a -> Trace a

example :: Waveform -> IO [SignalValue]
example wf = runTrace wf $ do
  clk <- load "clk"
  mySignal <- load "mySignal"
  sampleOn (inspect clk <&> \clkValue -> decodeInt clkValue == Just 1) $ do
    runInspect $ do
      myValue <- inspect mySignal 
      pure myValue
```

The above examples uses `sampleOn` to only sample time points where the clock signal is `1`.
