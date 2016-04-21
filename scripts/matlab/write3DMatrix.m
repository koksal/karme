function write3DMatrix(file, matrix)
  assert(length(size(matrix)) == 3);

  nbComponents = size(matrix, 1);
  nbIMF = size(matrix, 2);
  nbPoints = size(matrix, 3);

  resultH = {};
  resultH = [resultH, 'dim1'];
  resultH = [resultH, 'dim2'];
  resultH = [resultH, 'value'];
  result = [];

  for i = 1:nbComponents
    for j = 1:nbIMF
      for k = 1:nbPoints
        row = [i, j, matrix(i, j, k)];
        result = [result; row];
      end
    end
  end

  csvwriteh(file, result, resultH);
end
