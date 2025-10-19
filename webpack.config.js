const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');

module.exports = (env, argv) => {
    const isWeb = env && env.target === 'web';
    
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
            target: isWeb ? "webworker" : "node",
            entry: isWeb ? './lib/js/src/Main/Web.bs.js' : './lib/js/src/Main/Desktop.bs.js',
            output: {
                path: path.join(__dirname, 'dist'),
                filename: isWeb ? 'web.bundle.js' : 'app.bundle.js',
                libraryTarget: 'commonjs2',
            },
            devtool: 'source-map',
            externals: {
                vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
            },
            resolve: isWeb ? {
                alias: {
                    // Node.js core modules - disable for web (both old and new schemes)
                    'fs': false,
                    'node:fs': false,
                    'child_process': false,
                    'node:child_process': false,
                    'net': false,
                    'node:net': false,
                    'os': false,
                    'node:os': false,
                    'tls': false,
                    'node:tls': false,
                    'https': false,
                    'node:https': false,
                    'http': false,
                    'node:http': false,
                    'util': false,
                    'node:util': false,
                    // Desktop-only packages
                    'unzipper': false,
                    'getos': false,
                    'untildify': path.resolve(__dirname, 'untildify-shim.js'),
                },
                fallback: {
                    // Provide browser-compatible alternatives
                    'path': require.resolve('path-browserify'),
                    'node:path': require.resolve('path-browserify'),
                    'url': require.resolve('url'),
                    'node:url': require.resolve('url'),
                    'buffer': require.resolve('buffer'),
                    'process': require.resolve('process/browser'),
                    'stream': require.resolve('stream-browserify'),
                    'crypto': require.resolve('crypto-browserify'),
                }
            } : {},
            plugins: isWeb ? [
                // Handle Node.js core modules resolution
                new webpack.ProvidePlugin({
                    Buffer: ['buffer', 'Buffer'],
                    process: 'process/browser',
                }),
                // Add polyfill for Node.js core modules
                new webpack.NormalModuleReplacementPlugin(
                    /^node:/,
                    (resource) => {
                        const mod = resource.request.replace(/^node:/, '');
                        switch (mod) {
                            case 'path':
                                resource.request = 'path-browserify';
                                break;
                            case 'url':
                                resource.request = 'url';
                                break;
                            case 'fs':
                            case 'child_process':
                            case 'net':
                            case 'os':
                            case 'tls':
                            case 'https':
                            case 'http':
                            case 'util':
                                resource.request = path.resolve(__dirname, 'empty-module.js');
                                break;
                        }
                    }
                )
            ] : []
        }
    ];
};