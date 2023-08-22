const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyPlugin = require("copy-webpack-plugin");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");
const webpack = require("webpack");
const postcssPresetEnv = require("postcss-preset-env");

const API_URL = "https://api.unison.cloud/v1";
const UI_CORE_SRC = "elm-stuff/gitdeps/github.com/unisonweb/ui-core/src";
const WEBSITE_URL = process.env.WEBSITE_URL || "https://www.unison-lang.org";

const unisonCloud = {
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: [
          "style-loader",
          {
            loader: "css-loader",
            options: { importLoaders: 1 },
          },
          {
            loader: "postcss-loader",
            options: {
              postcssOptions: {
                plugins: [
                  postcssPresetEnv({
                    features: {
                      "is-pseudo-class": false,
                      "custom-media-queries": {
                        importFrom: `${UI_CORE_SRC}/css/ui/viewport.css`,
                      },
                    },
                  }),
                ],
              },
            },
          },
        ],
      },
      {
        test: /\.md$/i,
        type: "asset/source",
      },
      {
        test: /\.(png|svg|jpg|jpeg|gif)$/i,
        type: "asset/resource",
      },
      {
        test: /\.(woff(2)?|ttf|eot)$/i,
        type: "asset/resource",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "elm-asset-webpack-loader",
          },
          {
            loader: "elm-webpack-loader",
            options: {
              debug: false,
              cwd: __dirname,
            },
          },
        ],
      },
    ],
  },
  resolve: {
    alias: {
      assets: path.resolve(__dirname, "src/assets/"),
      "ui-core": path.resolve(__dirname, UI_CORE_SRC + "/"),
    },
  },

  entry: "./src/unisonCloud.js",

  plugins: [
    new HtmlWebpackPlugin({
      template: "./src/unisonCloud.ejs",
      inject: "body",
      publicPath: "/static/",
      base: "/",
      filename: path.resolve(__dirname, "dist/unisonCloud/index.html"),
    }),

    new FaviconsWebpackPlugin({
      logo: "./src/assets/favicon.svg",
      inject: true,
      favicons: {
        appName: "Unison Cloud",
        appDescription: "Write code. Hit run. The cloud computes.",
        developerName: "Unison",
        developerURL: "https://unison.cloud",
        background: "#5595F4",
        theme_color: "#5595F4",
      },
    }),

    new CopyPlugin({
      patterns: [
        {
          from: "src/assets/unison-cloud-social.png",
          to: "unison-cloud-social.png",
        },
        {
          from: "src/robots.txt",
          to: "robots.txt",
        },
      ],
    }),

    new webpack.DefinePlugin({
      API_URL: JSON.stringify(API_URL),
      WEBSITE_URL: JSON.stringify(WEBSITE_URL),
      APP_ENV: JSON.stringify("production"),
    }),
  ],

  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist/unisonCloud/static"),
    clean: true,
  },
};

module.exports = unisonCloud;
