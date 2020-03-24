const path = require('path');

module.exports = {
    target: "node",
    entry: './lib/js/src/View/ViewMain.bs.js',
    output: {
        path: path.join(__dirname, 'dist'),
        filename: 'bundled-view.js',
        libraryTarget: 'window',
    },
    devtool: 'source-map',
    module: {
        rules: [
            {
                test: /\.less$/,
                loader: 'less-loader', // compiles Less to CSS
            },
        ],
    },
};
