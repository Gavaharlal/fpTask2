# Usage
###Start server:
    stack run fpTask2-exe <port>
`port` argument is optional. Default port is 8285 and located in the **src/Config.hs** file.

###New client session:
    stack run fpTask2-front
When starting new client session you will be prompted to input address and port. Use *localhost* and *8225* if server is running on the same machine.   

    
###Test:
    stack test
Note that server must be started before running tests.
#
###Known issues:
If the terminal window is small, the interface elements may not fit.