clang -O3 c/code.c -o c/code
go build -ldflags "-s -w" -o go/code go/code.go
javac jvm/code.java
RUSTFLAGS="-Zlocation-detail=none" cargo +nightly build --manifest-path rust/Cargo.toml --release
kotlinc -include-runtime kotlin/code.kt -d kotlin/code.jar
#kotlinc-native -include-runtime kotlin/code.kt -d kotlin/code
dart compile exe dart/code.dart -o dart/code --target-os=macos
zig build-exe -O ReleaseFast -femit-bin=zig/code zig/code.zig
luajit -b lua/code.lua lua/code
swiftc -O -parse-as-library -Xcc -funroll-loops -Xcc -march=native -Xcc -ftree-vectorize -Xcc -ffast-math swift/code.swift -o swift/code
