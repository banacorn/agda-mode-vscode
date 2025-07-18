const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');

module.exports = (env, argv) => {
    return [
        {
            target: "node",
            entry: './lib/js/src/View/Root.bs.js',
            output: {
                path: path.join(__dirname, 'dist'),
                filename: 'bundled-view.js',
                libraryTarget: 'window',
            },
            devtool: 'source-map',
            externals: {
                vscode: 'vscode' // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
            },
            module: {
                rules: [
                    {
                        test: /\.less$/,
                        loader: 'less-loader', // compiles Less to CSS
                    },
                ],
            },
            plugins: [
                new CopyPlugin({
                    patterns: [
                        { from: 'asset/codicon', to: 'codicon' },
                    ],
                }),
            ],
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
            },
            resolve: {},
            plugins: []
        }
    ];
};