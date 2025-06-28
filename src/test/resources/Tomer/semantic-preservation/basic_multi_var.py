a = 1
#! Start of synth number: 0 of: x,y,z
#! 1) a = 1 => x = 2, y = 3, z = 4 
#! Start of synth number: 1 of: z
#! 1) a = 1 => z = 4 
z = a + a + 2
#! End of synth number: 1
x = a + a
y = a + x
#! End of synth number: 0


#note:
#the synth of x,y,z ordered x,y,z in diffrent way. we want to make sure the 
# order keeps, or atleast doesnt assign z: x+x
