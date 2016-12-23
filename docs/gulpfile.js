var gulp = require('gulp');
var shell = require('gulp-shell');

gulp.task('build-docs', shell.task('make clean html'));

gulp.task('default', ['build-docs'], function() {
    gulp.watch(['./source/**/*.rst'
               ,'./source/**/*.css'
               ,'./source/**/*.js'
               ,'./source/**/*.html'
               ,'./sources/**/*.py'], ['build-docs']);
});
