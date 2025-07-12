Basic framework for Visual Pinball X (VPX)

The goal is for a system meant for original tables which don't require VPinMAME, have some modern physics, and use some kind of reasonable software design. The use of VBScript and an API full of global variables doesn't make that last part easy.

## Setup
This is VBScript so it should work fine with Windows, but it was developed on Linux using WINE to run the VPX 10.8 Windows 32 bit version. A VPX build for Linux exists but it is not compatible with some systems and apparently does not contain the editor. The Windows version of the editor works with WINE though there are some quirks.

To setup the environment on Linux, read the excellent instructions at the link below, but use FlexDMD 1.9.1 instead of 1.8.0. (The DMDext version can remain 1.8.0.)

https://www.vpforums.org/index.php?showtopic=50740&p=525021

The instructions say that versions of FlexDMD newer than 1.8.0 won't work but  there weren't any newer versions released when they were written. 1.8.0 sometimes has index out of range errors when loading assets that are hopefully fixed in 1.9.1.

## Architecture
A single VPX file containing all the assets and game code is desirable for distribution. But thousands of lines of code in one file makes for unpleasant development. Therefore the code has been divided into separate files which are loaded at runtime. These can be combined for distribution and a build script will perhaps be created for that in the future.

The basic goal of the architecture is to separate hardware simulation things from the game software. Classes are used where possible to lessen pressure on the global namespace.
