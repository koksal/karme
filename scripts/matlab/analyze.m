function analyze(datafname, proteinsfname)

[headers, data] = csvreadh(datafname);

addpath('./drtoolbox')

transformNames = textread(proteinsfname, '%s', 'delimiter', '\n');

stepName = 'Step';
labelName = 'Pseudotime';

function index = findIndex(cell, element)
    for i = 1:length(cell)
        if (strcmp(cell{i}, element))
            index = i;
        end
    end
end

transformIndices = [];
for i = 1:length(transformNames)
    transformIndices = [transformIndices findIndex(headers, transformNames{i})];
end
stepIndex = findIndex(headers, stepName);
labelIndex = findIndex(headers, labelName);

stepCol = data(:,stepIndex);
labelCol = data(:,labelIndex);

dataOnly = data(:, transformIndices);

[Reduced, mapping] = compute_mapping(dataOnly, 't-SNE', 2);

a = 5;

plot2 = scatter(Reduced(:, 1), Reduced(:, 2), a, stepCol, 'filled');
waitfor(plot2);
plot2 = scatter(Reduced(:, 1), Reduced(:, 2), a, labelCol, 'filled');
waitfor(plot2);

end
