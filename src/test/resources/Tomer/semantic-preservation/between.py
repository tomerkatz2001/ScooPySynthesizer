#! Start of synth number: 48 of: rs
#! 1) a = 1, b = 2, c = 4 => rs = True 
#! 2) a = 3, b = 1, c = 10 => rs = False 
if a > b:
    rs = False
else:
    #! Start of synth number: 51 of: rs
    #! 1) a = 1, b = 2, c = 4 => rs = True 
    #! 2) a = 1, b = 10, c = 4 => rs = False 
    if b > c:
        rs = False
    else:
        rs = a > -1
    #! End of synth number: 51
         
    #! End of synth number: 48



#just the examples:
# #! Start of synth number: 52 of: rs
# #! 1) a = 1, b = 2, c = 4 => rs = True 
# #! 2) a = 3, b = 1, c = 10 => rs = False 
# #! 3) a = 1, b = 10, c = 4 => rs = False 
# if b > c:
#     rs = False
# else:
#     rs = b > a
# #! End of synth number: 52