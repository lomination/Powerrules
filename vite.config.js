import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  plugins: [scalaJSPlugin()],
  root: 'js/src/main/resources',
  build: {
    outDir: '../../../../dist'
  }
});