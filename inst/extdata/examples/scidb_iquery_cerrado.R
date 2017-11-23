
# cerrado array name
array_name <- "cerrado_20171111"

cerrado_range <- list()
cerrado_range$col_from <- 57936
cerrado_range$col_to <- 67135
cerrado_range$row_from <- 44033
cerrado_range$row_to <- 55232

# create array
cerrado_array <- "CREATE ARRAY %s <predicted: int32, rentropy: double> [col_id=%s:%s, 40, 0, row_id=%s:%s, 40, 0, period_id=1:*, 40, 0]"
sprintf(cerrado_array,
        array_name,
        cerrado_range$col_from, cerrado_range$col_to,
        cerrado_range$row_from, cerrado_range$row_to)

# generate process
cerrado_process <- "redimension(apply(stream(cast(apply(project(between(mod13q1_512, %s, %s, 0, %s, %s, 512), ndvi, evi, nir, mir), x, double(col_id), y, double(row_id), t, double(time_id)), <ndvi:int32, evi:int32, nir:int32, mir:int32, x:double, y:double, t:double>[col_id=0:172799, 40, 0, row_id=0:86399, 40, 0, time_id=0:511, 512, 0]), 'Rscript /net/esensing-001/disks/d9/scidb15_12/scripts/cerrado/classify_scidb.R cores=%s', 'format=df', 'types=int32,double,double,int32,double,double,int32,double', 'names=sample_id,x,y,t,from,to,predicted,rentropy'), col_id, int64(x), row_id, int64(y), period_id, int64(t)), <predicted:int32, rentropy:double>[col_id = %s:%s, 40,  0, row_id = %s:%s, 40, 0, period_id = 1:*, 40, 0])"


chunk <- Cerrado_SCiDB_chunks[1,]
sprintf(cerrado_iquery,
        chunk$col_from, chunk$row_from, chunk$col_to, chunk$row_to, 2,
        chunk$col_from, chunk$col_to, chunk$row_from, chunk$row_to)
