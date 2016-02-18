function fcs_to_csv(dirname)
dirname
addpath('fca_readfcs')
filepattern = strcat(dirname, '/*.fcs')
files = dir(filepattern)
disp(files)
for i = 1:length(files)
    fname = strcat(dirname, '/', files(i).name);
    disp(fname);
    outfname = strcat(fname, '.csv');
    disp(outfname);
    [fcsdat, fcshdr, fcsdatscaled, fcsdatcomp] = fca_readfcs(fname);
    params = fcshdr.par;
    labels = {};
    for i = 1:length(params)
        labels = [labels, params(i).name];
    end
    csvwriteh(outfname, fcsdatscaled, labels);
end
