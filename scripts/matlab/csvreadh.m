function [h,m] = csvreadh( filename, delim )
%CSVREADH Read a comma separated value file with header.
%   [H,M] = CSVREADH('FILENAME') reads a comma separated value formatted file
%   FILENAME.  The result data is returned in M, the header in H. 
%   The file can only contain numeric values as data and a string for 
%   the header.
%   Taken from: https://www.nesono.com/node/415

% Validate input args
if nargin==0
    error(nargchk(1,1,nargin,'struct')); 
end

% Get Filename
if ~ischar(filename)
    error('csvreadh:FileNameMustBeString', ...
        'Filename must be a string.'); 
end

% Make sure file exists
if exist(filename,'file') ~= 2 
    error('csvreadh:FileNotFound',...
    'File not found.');
end

if nargin==1
    delim = ',';
end

% open input file
file = fopen( filename );
line = fgetl( file );
h = regexp( line, delim, 'split' );

fclose(file);

m = csvread(filename, 1, 0);
