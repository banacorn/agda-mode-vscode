const path = require('path');

module.exports = {
    target: "node",
    entry: './lib/js/src/View/Panel.bs.js',
    output: {
        path: path.join(__dirname, 'dist'),
        filename: 'bundled-view.js',
        libraryTarget: 'window',
    },
    devtool: 'source-map'
};
