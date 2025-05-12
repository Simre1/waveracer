#!/bin/bash

set -x

rm libwellen_binding.so

cd wellen-binding
cargo build --release
cbindgen --crate wellen-binding --output ../wellen-binding.h
cd ../

cp wellen-binding/target/release/libwellen_binding.so libwellen_binding.so

echo "Done"
