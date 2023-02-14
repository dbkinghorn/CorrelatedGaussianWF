function NA = norma(A)

% normailize the columns of A
sizeA = size(A);
NA = zeros(sizeA);
nc = sizeA(2);

for i=1:nc
	NA(:,i) = A(:,i)/sqrt( A(:,i)'*A(:,i) );
end
