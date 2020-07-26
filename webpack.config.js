const path = require('path');

module.exports = [
    {
        target: "node",
        entry: './lib/js/src/View/ViewMain.bs.js',
        output: {
            path: path.join(__dirname, 'dist'),
            filename: 'view.bundle.js',
            libraryTarget: 'window',
        },
        devtool: 'source-map',
        externals: {
            vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
        },
        module: {
            rules: [
                {
                    test: /\.less$/,
                    loader: 'less-loader', // compiles Less to CSS
                },
            ],
        },
    },
    {
        target: "node",
        entry: './lib/js/src/Main.bs.js',
        output: {
            path: path.join(__dirname, 'dist'),
            filename: 'app.bundle.js',
            libraryTarget: 'commonjs2',
        },
        devtool: 'source-map',
        externals: {
            vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
        }
    }
];