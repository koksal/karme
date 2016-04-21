function run_memd(inFile, outFile)
  addpath('./scripts/matlab/memd')

  % read matrix from inFile
  data = csvread(inFile);

  result = memd(data);

  % write MEMD output to outFile
  write3DMatrix(outFile, result);
end
