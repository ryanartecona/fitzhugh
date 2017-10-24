const path = require('path');
const webpack = require('webpack');
const WriteFilePlugin = require('write-file-webpack-plugin');

module.exports = {
  entry: {
    main: './lib/js/src/main'
  },
  output: {
    filename: '[name].js',
    path: path.join(__dirname, './public'),
    publicPath: '/public'
  },
  devServer: {
    contentBase: path.resolve(__dirname, 'public'),
    port: 8000,
    stats: {
      modules: false
    }
  },
  plugins: [new WriteFilePlugin()],
  stats: {
    modules: false
  }
};
