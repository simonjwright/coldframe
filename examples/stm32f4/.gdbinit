echo Setting the target...\n
target remote :4242
echo Setting the file...\n
load simple_buttons

#define hookpost-load
#echo Resetting the processor and peripherals...\n
#set *0xE000ED0C := 0x05FA0004
#echo Reset complete\n
#end
