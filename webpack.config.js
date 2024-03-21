const path = require('path');

module.exports = {
  entry: './jsbits/index.js',
  output: {
    path: path.resolve(__dirname, 'dist-newstyle'),
    filename: 'index.bundle.js',
  },
  resolve: {
    extensions: ['.ts', '.js'],
  },
  devtool: false,
};