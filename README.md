# Minifloat

Based off https://github.com/ISSOtm/gb-starter-kit with some modifications:
* `header.asm` - commented out bank 0 asserts

## Compiling

Simply open you favorite command prompt / terminal, place yourself in this directory (the one the Makefile is located in), and run the command `make`. This should create a bunch of things, including the output in the `bin` folder.

While this project is able to compile under "bare" Windows (i.e. without using MSYS2, Cygwin, etc.), it requires PowerShell, and is sometimes unreliable. You should try running `make` two or three times if it errors out.

If you get errors that you don't understand, try running `make clean`. If that gives the same error, try deleting the `deps` folder. If that still doesn't work, try deleting the `bin` and `obj` folders as well. If that still doesn't work, you probably did something wrong yourself.
