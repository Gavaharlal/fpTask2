# Usage
###Start server:

    stack run fpTask2-exe <port>
    
`port` argument is optional. Default port is 8285 and located in the **src/Config.hs** file.

###New client session:

    stack run fpTask2-front
    
When starting new client session you will be prompted to input address and port. Use *localhost* and *8225* if server is running on the same machine.   

    
###Test:

    stack test
    
Note that the server must be started on default port before running tests.
#

###Make:
You can use make both for testing and launching the server.<br>  
Use `make` or `make run-test` for testing.  
Use `make run-app` to launch a new game session (server will be started automatically).

#
###Known issues:
If the terminal window is small, the interface elements may not fit.