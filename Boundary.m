data = csvread('region_2_train_coord.csv',1,1);
x = data(:,1);
y = data(:,2);
%k = boundary(x,y);
plot(x,y,'.');
hold on
%plot(x(k),y(k));
k = boundary(x,y,1);
plot(x(k),y(k));
%k = boundary(x,y,1);
%plot(x(k),y(k));
fdaPDEBoundary = zeros(length(k)-1,2);
for i=1:(length(k)-1)
    fdaPDEBoundary(i,1) = k(i);
    fdaPDEBoundary(i,2) = k(i+1);
end
csvwrite('region_2_train_boundary.csv',fdaPDEBoundary)
