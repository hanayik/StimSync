function pulsatingOutputTest
pause(2);
s1 = SerDeviceIndex;
SerDOut(s1,1,0);
p = 0.001;
n = 100;
for i = 1:n
    SerDOut(s1,1,127);
    pause(p);
    SerDOut(s1,1,0);
    pause(p);
end
fclose(s1);
end