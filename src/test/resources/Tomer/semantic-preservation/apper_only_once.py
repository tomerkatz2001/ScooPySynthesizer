def g(a):

    #! Start of synth number: 12 of: _set,rs
    #! 1) a = [4, 5, 9, 22, 3, 22, 9] => _set = {3, 4, 5, 22, 9}, rs = False 
    #! 2) a = [4, 5, 9, 22, 3] => _set = {3, 4, 5, 22, 9}, rs = True 
    #! 3) a = [4, 5, 9, 22, 3, 6] => _set = {3, 4, 5, 6, 22, 9}, rs = True 

    _set = set(a)

    #! Start of synth number: 13 of: rs
    #! 1) a = [4, 5, 9, 22, 3, 22, 9], _set = {3, 4, 5, 22, 9} => rs = False 
    #! 2) a = [4, 5, 9, 22, 3], _set = {3, 4, 5, 22, 9} => rs = True 
    #! 3) a = [4, 5, 9, 22, 3, 6], _set = {3, 4, 5, 6, 9, 22} => rs = True 
    rs = len(a) == len(_set)
    #! End of synth number: 13

    #! End of synth number: 12