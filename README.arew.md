# Arew

**experimental work-in-progress, use it at your own risk.**

## Getting started

```sh
./configure --pb
make ta6le.bootquick
./configure --kernelobj
make
```

Then copy `arew` in `/usr/local/bin/`:

```sh
cp ta6le/bin/ta6le/arew /usr/local/bin/
```

To compile `example.scm` into `example`, you can do:

```sh
arew compile example.scm example
```
