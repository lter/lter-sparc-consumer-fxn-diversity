# CFD Tools

Scripts are meant to make interactions with data more _repeatable_ and _reproducible_, however, repeated operations within a given script should be avoided as much as possible. Instead, it's good practice to write smaller scripts that contain custom functions to do those operations. The 'actual' scripts can then load/use those functions to avoid repeating the operations inside of the functions and thus dodge the potential for error that comes with any duplication.

Scripts in this folder starting with `fxn_` contain one function each with a description of its purpose and arguments in the top of the file.
