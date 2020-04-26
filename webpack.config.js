const path = require("path");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");

const outputPath = path.join(__dirname, "bundleOutput");

module.exports = {
  entry: "./src/Index.bs.js",
  // If you ever want to use webpack during development, change 'production'
  // to 'development' as per webpack documentation. Again, you don't have to
  // use webpack or any other bundler during development! Recheck README if
  // you didn't know this
  mode: "development",
  output: {
    path: outputPath,
    filename: "index.js",
  },
  resolveLoader: {
    modules: ["node_modules"],
  },
  devServer: {
    contentBase: __dirname,
  },
  // plugins: [new CleanWebpackPlugin()],
};
