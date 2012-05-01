int 13
mov ah bh
mov 0 dh
mov 1 ah
cmp bh dh
label1:
add 1 dh
mul dh
cmp bh dh
jg label1
int 11
int 10
