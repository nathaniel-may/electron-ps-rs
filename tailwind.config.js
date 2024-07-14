const defaultTheme = require("tailwindcss/defaultTheme");
const colors = require("tailwindcss/colors");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.purs", "./src/*.html"],
  theme: {
    extend: {},
  },
  plugins: [],
};
