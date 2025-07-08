const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = [
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
        }
    },
    // {
    //     target: "webworker",
    //     entry: './lib/js/src/Main.bs.js',
    //     output: {
    //         path: path.join(__dirname, 'dist'),
    //         filename: 'web.bundle.js',
    //         libraryTarget: 'commonjs2',
    //     },
    //     devtool: 'source-map',
    //     externals: {
    //         vscode: 'commonjs vscode'
    //     },
    //     resolve: {
    //         fallback: {
    //             path: require.resolve('path-browserify'),
    //             url: require.resolve('url'),
    //             buffer: require.resolve('buffer'),
    //             process: require.resolve('process/browser'),
    //             stream: require.resolve('stream-browserify'),
    //             crypto: require.resolve('crypto-browserify'),
    //             fs: false,
    //             os: false,
    //             child_process: false,
    //             net: false,
    //             tls: false,
    //             https: false,
    //             http: false
    //         }
    //     }
    // }
];