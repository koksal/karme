function run_bemd(inFile, xOutFile, yOutFile)

  % read the two columns
  data = csvread(inFile);
  assert(size(data, 2) == 2);

  x = data(:,1);
  y = data(:,2);

  T = [];
  input = complex(x, y);
  nbIter = 10;
  maxIMFs = [];
  nbDirs = 32;

  [imf, nb] = cemdc2_fix(T, input, nbIter, maxIMFs, nbDirs);
  % [imf, nb] = emd(input);

  xIMFs = real(imf);
  yIMFs = imag(imf);

  csvwrite(xOutFile, xIMFs');
  csvwrite(yOutFile, yIMFs');
end
