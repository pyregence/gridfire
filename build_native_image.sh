#!/usr/bin/env bash

if [ -z "$GRAALVM_HOME" ]; then
    echo "Please set \$GRAALVM_HOME"
    exit 1
fi

if [ ! -e "$GRAALVM_HOME/bin/native-image" ]; then
    echo "Installing the native-image command..."
    "$GRAALVM_HOME/bin/gu" install native-image
fi

echo "Compiling Clojure to Java bytecode..."
clojure -M -e "(compile 'gridfire.gen-raster)"

"$GRAALVM_HOME/bin/native-image" \
    -cp "$(clojure -Spath)":classes \
    -H:Name=gridfire-native-image \
    -H:+ReportExceptionStackTraces \
    --verbose \
    --no-fallback \
    --initialize-at-build-time=java.awt.image.Raster,org.geotools.coverage.grid.GridGeometry2D,sun.java2d.StateTrackableDelegate$2,java.awt.image.DataBuffer,java.awt.Image,org.geotools.referencing.operation.transform.AbstractMathTransform,org.geotools.coverage.grid.AbstractGridCoverage,javax.media.jai.RenderedOp,java.awt.RenderingHints,java.awt.Toolkit,sun.awt.image.SunWritableRaster,java.awt.image.WritableRaster,java.awt.color.ColorSpace$BuiltInSpace,org.opengis.referencing.datum.PixelInCell,org.geotools.referencing.operation.projection.MapProjection,org.geotools.coverage.grid.GridCoverage2D,org.geotools.coverage.GridSampleDimension,org.geotools.coverage.grid.RenderedSampleDimension,java.awt.image.DataBufferByte,org.geotools.util.logging.Logging,java.awt.Dimension,java.awt.image.ColorModel,org.geotools.referencing.wkt.Formattable,java.awt.image.IndexColorModel,javax.media.jai.JAI,sun.java2d.StateTrackableDelegate,org.geotools.coverage.grid.GeneralGridGeometry,java.awt.image.SampleModel \
    gridfire.gen_raster



#    --trace-class-initialization=java.awt.image.Raster,org.geotools.coverage.grid.GridGeometry2D,sun.java2d.StateTrackableDelegate$2,java.awt.image.DataBuffer,java.awt.Image,org.geotools.referencing.operation.transform.AbstractMathTransform,org.geotools.coverage.grid.AbstractGridCoverage,javax.media.jai.RenderedOp,java.awt.RenderingHints,java.awt.Toolkit,sun.awt.image.SunWritableRaster,java.awt.image.WritableRaster,java.awt.color.ColorSpace$BuiltInSpace,org.opengis.referencing.datum.PixelInCell,org.geotools.referencing.operation.projection.MapProjection,org.geotools.coverage.grid.GridCoverage2D,org.geotools.coverage.GridSampleDimension,org.geotools.coverage.grid.RenderedSampleDimension,java.awt.image.DataBufferByte,org.geotools.util.logging.Logging,java.awt.Dimension,java.awt.image.ColorModel,org.geotools.referencing.wkt.Formattable,java.awt.image.IndexColorModel,javax.media.jai.JAI,sun.java2d.StateTrackableDelegate,org.geotools.coverage.grid.GeneralGridGeometry,java.awt.image.SampleModel \

    # Used with errors:
    # --allow-incomplete-classpath \
    # --initialize-at-build-time="" \
    # --diagnostics-mode \
    # --initialize-at-run-time=com.sun.media.imageioimpl.plugins.tiff.TIFFCodecLibFaxDecompressor \
    # --initialize-at-run-time=com.sun.media.imageioimpl.plugins.pnm.PNMImageReader \
    # --initialize-at-run-time=org.geotools.referencing.factory.DeferredAuthorityFactory \
    # --enable-all-security-services \
    # --trace-object-instantiation=java.util.TimerThread \
    # --trace-object-instantiation=java.io.RandomAccessFile \
    # --trace-object-instantiation=org.geotools.util.WeakCollectionCleaner \
    # --trace-object-instantiation=it.geosolutions.imageio.utilities.WeakCollectionCleaner \
    # --initialize-at-run-time=it.geosolutions.imageio.utilities.SoftValueHashMap \
    # --trace-object-instantiation=java.util.zip.Inflater \
    # --initialize-at-run-time=org.apache.logging.log4j.core.async.AsyncLoggerContext \
    # --initialize-at-run-time=org.apache.logging.log4j.core.config.yaml.YamlConfiguration \
    # --initialize-at-run-time=org.apache.logging.log4j.core.pattern.JAnsiTextRenderer \
    # --initialize-at-run-time=com.sun.media.imageioimpl.plugins.pnm.PNMImageWriter \
    # --trace-object-instantiation=com.sun.jmx.mbeanserver.JmxMBeanServer \
    # --trace-object-instantiation=java.util.Random \
    # --initialize-at-run-time=org.geotools.referencing.crs.DefaultGeographicCRS \
    # --initialize-at-run-time=org.geotools.referencing.operation.transform.ProjectiveTransform$Affine \
    # --initialize-at-run-time=javax.management.MBeanServerFactory \
    # --initialize-at-run-time=org.geotools.util.WeakValueHashMap \
    # --trace-class-initialization=org.geotools.util.WeakValueHashMap \
    # --initialize-at-run-time=com.sun.jmx.mbeanserver.JmxMBeanServer \
    # --initialize-at-run-time=org.geotools.referencing.datum.DefaultGeodeticDatum \

    # Unused:
    # "-J-Dclojure.compiler.direct-linking=true" \
    # --no-server
    # --report-unsupported-elements-at-runtime \
    # "-J-Xmx100G" \
    # -H:ConfigurationFileDirectories=. \
    # org.geotools.util.SoftValueHashMap
    # --initialize-at-run-time=org.geotools.referencing.factory.DeferredAuthorityFactory.TIMER \
    # --trace-class-initialization=javax.management.MBeanServerFactory \

