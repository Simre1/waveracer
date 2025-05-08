#!/bin/bash

set -x

rm libwellen_binding.so

cd wellen-binding
cargo build
cbindgen --crate wellen-binding --output ../wellen-binding.h
cd ../

cp wellen-binding/target/debug/libwellen_binding.so libwellen_binding.so

echo "Done"
