var gulp = require("gulp");
var browserSync = require("browser-sync").create();
var shell = require("gulp-shell");

gulp.task("serve", ["parsets-docs"], function() {
  browserSync.init({
    server: "dist/parsets-docs"
  });

  gulp.watch("BlocklandGlass/**/*.{cs,gui}", ["parsets-docs"]);
  gulp.watch("src/**/*.hs", ["parsets-docs"]);
  gulp.watch("dist/parsets-docs/**/*.*").on("change", browserSync.reload);
});

gulp.task("parsets-docs", function() {
  return gulp.src("")
    .pipe(shell("cabal run -- docs BlocklandGlass"))
});

gulp.task("default", ["serve"]);
