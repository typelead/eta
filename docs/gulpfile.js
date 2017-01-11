var gulp = require('gulp');
var shell = require('gulp-shell');

gulp.task('highlight', shell.task('cd highlight; python setup.py install'));
gulp.task('default', ['highlight'], shell.task('make clean html'));

gulp.task('reload', ['default'], function() {
    gulp.watch(['./source/**/*.rst'
               ,'./source/**/*.css'
               ,'./source/**/*.js'
               ,'./source/**/*.html'
               ,'./source/**/*.conf'
               ,'./source/**/*.py'
               ,'./highlight/**/*.py'], ['default']);
});
