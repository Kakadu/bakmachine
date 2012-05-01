mov 0,dh
mov 5,bh
mov 1,ah
cmp dh,bh
label1:
add 1,dh
mul dh
cmp dh,bh
jl label1
int 10
