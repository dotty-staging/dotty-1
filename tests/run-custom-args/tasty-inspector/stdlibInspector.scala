import scala.quoted._
import scala.tasty.inspector._

@main def Test = {
  val inspector = new TastyInspector {
    protected def processCompilationUnit(using QuoteContext)(tree: qctx.tasty.Tree): Unit = {
      println(tree.show)
      println(tree.showExtractors)
    }
  }
  val classes = List[String](
    "dotty.DottyPredef",
    "dotty.internal.CompileTimeMacros",
    "dotty.internal.StringContextMacro",
    "dotty.runtime.Arrays",
    "dotty.runtime.LazyVals",
    "scala.$eq$colon$eq",
    "scala.$less$colon$less",
    "scala.$times$colon",
    "scala.annotation.alpha",
    "scala.annotation.Annotation",
    "scala.annotation.ClassfileAnnotation",
    "scala.annotation.compileTimeOnly",
    "scala.annotation.ConstantAnnotation",
    "scala.annotation.constructorOnly",
    "scala.annotation.elidable",
    "scala.annotation.implicitAmbiguous",
    "scala.annotation.implicitNotFound",
    "scala.annotation.infix",
    "scala.annotation.internal.Alias",
    "scala.annotation.internal.AnnotationDefault",
    "scala.annotation.internal.Body",
    "scala.annotation.internal.Child",
    "scala.annotation.internal.ContextResultCount",
    "scala.annotation.internal.InlineParam",
    "scala.annotation.internal.Repeated",
    "scala.annotation.internal.sharable",
    "scala.annotation.internal.SourceFile",
    "scala.annotation.internal.unshared",
    "scala.annotation.internal.WithBounds",
    "scala.annotation.meta.beanGetter",
    "scala.annotation.meta.beanSetter",
    "scala.annotation.meta.companionClass",
    "scala.annotation.meta.companionMethod",
    "scala.annotation.meta.companionObject",
    "scala.annotation.meta.field",
    "scala.annotation.meta.getter",
    "scala.annotation.meta.languageFeature",
    "scala.annotation.meta.package",
    "scala.annotation.meta.param",
    "scala.annotation.meta.setter",
    "scala.annotation.migration",
    "scala.annotation.nowarn",
    "scala.annotation.RefiningAnnotation",
    "scala.annotation.showAsInfix",
    "scala.annotation.static",
    "scala.annotation.StaticAnnotation",
    "scala.annotation.strictfp",
    "scala.annotation.superTrait",
    "scala.annotation.switch",
    "scala.annotation.tailrec",
    "scala.annotation.threadUnsafe",
    "scala.annotation.TypeConstraint",
    "scala.annotation.unchecked.uncheckedStable",
    "scala.annotation.unchecked.uncheckedVariance",
    "scala.annotation.unspecialized",
    "scala.annotation.unused",
    "scala.annotation.varargs",
    "scala.AnyValCompanion",
    "scala.App",
    "scala.Array",
    "scala.beans.BeanProperty",
    "scala.beans.BooleanBeanProperty",
    "scala.Boolean",
    "scala.Byte",
    "scala.Char",
    "scala.collection.AbstractIndexedSeqView",
    "scala.collection.AbstractIterable",
    "scala.collection.AbstractIterator",
    "scala.collection.AbstractMap",
    "scala.collection.AbstractMapView",
    "scala.collection.AbstractSeq",
    "scala.collection.AbstractSeqView",
    "scala.collection.AbstractSet",
    "scala.collection.AbstractView",
    "scala.collection.AnyStepper",
    "scala.collection.ArrayOps",
    "scala.collection.BitSet",
    "scala.collection.BitSetOps",
    "scala.collection.BufferedIterator",
    "scala.collection.BuildFrom",
    "scala.collection.BuildFromLowPriority1",
    "scala.collection.BuildFromLowPriority2",
    "scala.collection.ClassTagIterableFactory",
    "scala.collection.ClassTagSeqFactory",
    "scala.collection.concurrent.CNode",
    "scala.collection.concurrent.FailedNode",
    "scala.collection.concurrent.INode",
    "scala.collection.concurrent.KVNode",
    "scala.collection.concurrent.LNode",
    "scala.collection.concurrent.Map",
    "scala.collection.concurrent.RDCSS_Descriptor",
    "scala.collection.concurrent.SNode",
    "scala.collection.concurrent.TNode",
    "scala.collection.concurrent.TrieMap",
    "scala.collection.concurrent.TrieMapIterator",
    "scala.collection.concurrent.TrieMapSerializationEnd",
    "scala.collection.convert.AsJavaConverters",
    "scala.collection.convert.AsJavaExtensions",
    "scala.collection.convert.AsScalaConverters",
    "scala.collection.convert.AsScalaExtensions",
    "scala.collection.convert.impl.AnyBinaryTreeStepper",
    "scala.collection.convert.impl.AnyChampStepper",
    "scala.collection.convert.impl.AnyIndexedSeqStepper",
    "scala.collection.convert.impl.AnyIteratorStepper",
    "scala.collection.convert.impl.AnyNumericRangeStepper",
    "scala.collection.convert.impl.AnyTableStepper",
    "scala.collection.convert.impl.AnyVectorStepper",
    "scala.collection.convert.impl.BinaryTreeStepper",
    "scala.collection.convert.impl.BinaryTreeStepperBase",
    "scala.collection.convert.impl.BitSetStepper",
    "scala.collection.convert.impl.BoxedBooleanArrayStepper",
    "scala.collection.convert.impl.ChampStepperBase",
    "scala.collection.convert.impl.CharStringStepper",
    "scala.collection.convert.impl.CodePointStringStepper",
    "scala.collection.convert.impl.DoubleArrayStepper",
    "scala.collection.convert.impl.DoubleBinaryTreeStepper",
    "scala.collection.convert.impl.DoubleChampStepper",
    "scala.collection.convert.impl.DoubleIndexedSeqStepper",
    "scala.collection.convert.impl.DoubleIteratorStepper",
    "scala.collection.convert.impl.DoubleTableStepper",
    "scala.collection.convert.impl.DoubleVectorStepper",
    "scala.collection.convert.impl.IndexedStepperBase",
    "scala.collection.convert.impl.InOrderStepperBase",
    "scala.collection.convert.impl.IntArrayStepper",
    "scala.collection.convert.impl.IntBinaryTreeStepper",
    "scala.collection.convert.impl.IntChampStepper",
    "scala.collection.convert.impl.IntIndexedSeqStepper",
    "scala.collection.convert.impl.IntIteratorStepper",
    "scala.collection.convert.impl.IntNumericRangeStepper",
    "scala.collection.convert.impl.IntTableStepper",
    "scala.collection.convert.impl.IntVectorStepper",
    "scala.collection.convert.impl.IteratorStepperBase",
    "scala.collection.convert.impl.LongArrayStepper",
    "scala.collection.convert.impl.LongBinaryTreeStepper",
    "scala.collection.convert.impl.LongChampStepper",
    "scala.collection.convert.impl.LongIndexedSeqStepper",
    "scala.collection.convert.impl.LongIteratorStepper",
    "scala.collection.convert.impl.LongNumericRangeStepper",
    "scala.collection.convert.impl.LongTableStepper",
    "scala.collection.convert.impl.LongVectorStepper",
    "scala.collection.convert.impl.ObjectArrayStepper",
    "scala.collection.convert.impl.RangeStepper",
    "scala.collection.convert.impl.TableStepperBase",
    "scala.collection.convert.impl.VectorStepperBase",
    "scala.collection.convert.impl.WidenedByteArrayStepper",
    "scala.collection.convert.impl.WidenedCharArrayStepper",
    "scala.collection.convert.impl.WidenedFloatArrayStepper",
    "scala.collection.convert.impl.WidenedShortArrayStepper",
    "scala.collection.convert.ImplicitConversions",
    "scala.collection.convert.ImplicitConversionsToJava",
    "scala.collection.convert.ImplicitConversionsToScala",
    "scala.collection.convert.JavaCollectionWrappers",
    "scala.collection.convert.StreamExtensions",
    "scala.collection.convert.ToJavaImplicits",
    "scala.collection.convert.ToScalaImplicits",
    "scala.collection.DefaultMap",
    "scala.collection.DoubleStepper",
    "scala.collection.EvidenceIterableFactory",
    "scala.collection.EvidenceIterableFactoryDefaults",
    "scala.collection.Factory",
    "scala.collection.generic.BitOperations",
    "scala.collection.generic.DefaultSerializable",
    "scala.collection.generic.DefaultSerializationProxy",
    "scala.collection.generic.IsIterable",
    "scala.collection.generic.IsIterableLowPriority",
    "scala.collection.generic.IsIterableOnce",
    "scala.collection.generic.IsIterableOnceLowPriority",
    "scala.collection.generic.IsMap",
    "scala.collection.generic.IsSeq",
    "scala.collection.generic.package",
    "scala.collection.generic.SerializeEnd",
    "scala.collection.generic.Subtractable",
    "scala.collection.Hashing",
    "scala.collection.immutable.$colon$colon",
    "scala.collection.immutable.AbstractMap",
    "scala.collection.immutable.AbstractSeq",
    "scala.collection.immutable.AbstractSet",
    "scala.collection.immutable.AnyVectorStepper",
    "scala.collection.immutable.ArraySeq",
    "scala.collection.immutable.BigVector",
    "scala.collection.immutable.BitmapIndexedMapNode",
    "scala.collection.immutable.BitmapIndexedSetNode",
    "scala.collection.immutable.BitSet",
    "scala.collection.immutable.ChampBaseIterator",
    "scala.collection.immutable.ChampBaseReverseIterator",
    "scala.collection.immutable.DoubleVectorStepper",
    "scala.collection.immutable.HashCollisionMapNode",
    "scala.collection.immutable.HashCollisionSetNode",
    "scala.collection.immutable.HashMap",
    "scala.collection.immutable.HashMapBuilder",
    "scala.collection.immutable.HashSet",
    "scala.collection.immutable.HashSetBuilder",
    "scala.collection.immutable.IndexedSeq",
    "scala.collection.immutable.IndexedSeqDefaults",
    "scala.collection.immutable.IndexedSeqOps",
    "scala.collection.immutable.IntMap",
    "scala.collection.immutable.IntMapEntryIterator",
    "scala.collection.immutable.IntMapIterator",
    "scala.collection.immutable.IntMapKeyIterator",
    "scala.collection.immutable.IntMapUtils",
    "scala.collection.immutable.IntMapValueIterator",
    "scala.collection.immutable.IntVectorStepper",
    "scala.collection.immutable.Iterable",
    "scala.collection.immutable.LazyList",
    "scala.collection.immutable.LinearSeq",
    "scala.collection.immutable.LinearSeqOps",
    "scala.collection.immutable.List",
    "scala.collection.immutable.ListMap",
    "scala.collection.immutable.ListMapBuilder",
    "scala.collection.immutable.ListSet",
    "scala.collection.immutable.LongMap",
    "scala.collection.immutable.LongMapEntryIterator",
    "scala.collection.immutable.LongMapIterator",
    "scala.collection.immutable.LongMapKeyIterator",
    "scala.collection.immutable.LongMapUtils",
    "scala.collection.immutable.LongMapValueIterator",
    "scala.collection.immutable.LongVectorStepper",
    "scala.collection.immutable.Map",
    "scala.collection.immutable.MapBuilderImpl",
    "scala.collection.immutable.MapKeyIterator",
    "scala.collection.immutable.MapKeyValueTupleHashIterator",
    "scala.collection.immutable.MapKeyValueTupleIterator",
    "scala.collection.immutable.MapKeyValueTupleReverseIterator",
    "scala.collection.immutable.MapNode",
    "scala.collection.immutable.MapNodeRemoveAllSetNodeIterator",
    "scala.collection.immutable.MapOps",
    "scala.collection.immutable.MapValueIterator",
    "scala.collection.immutable.NewVectorIterator",
    "scala.collection.immutable.Nil",
    "scala.collection.immutable.Node",
    "scala.collection.immutable.NumericRange",
    "scala.collection.immutable.package",
    "scala.collection.immutable.Queue",
    "scala.collection.immutable.Range",
    "scala.collection.immutable.RangeIterator",
    "scala.collection.immutable.RedBlackTree",
    "scala.collection.immutable.Seq",
    "scala.collection.immutable.SeqMap",
    "scala.collection.immutable.SeqOps",
    "scala.collection.immutable.Set",
    "scala.collection.immutable.SetBuilderImpl",
    "scala.collection.immutable.SetHashIterator",
    "scala.collection.immutable.SetIterator",
    "scala.collection.immutable.SetNode",
    "scala.collection.immutable.SetOps",
    "scala.collection.immutable.SetReverseIterator",
    "scala.collection.immutable.SortedMap",
    "scala.collection.immutable.SortedMapOps",
    "scala.collection.immutable.SortedSet",
    "scala.collection.immutable.SortedSetOps",
    "scala.collection.immutable.Stream",
    "scala.collection.immutable.StrictOptimizedMapOps",
    "scala.collection.immutable.StrictOptimizedSeqOps",
    "scala.collection.immutable.StrictOptimizedSetOps",
    "scala.collection.immutable.StrictOptimizedSortedMapOps",
    "scala.collection.immutable.StrictOptimizedSortedSetOps",
    "scala.collection.immutable.TreeMap",
    "scala.collection.immutable.TreeSeqMap",
    "scala.collection.immutable.TreeSet",
    "scala.collection.immutable.Vector",
    "scala.collection.immutable.Vector0",
    "scala.collection.immutable.Vector1",
    "scala.collection.immutable.Vector2",
    "scala.collection.immutable.Vector3",
    "scala.collection.immutable.Vector4",
    "scala.collection.immutable.Vector5",
    "scala.collection.immutable.Vector6",
    "scala.collection.immutable.VectorBuilder",
    "scala.collection.immutable.VectorImpl",
    "scala.collection.immutable.VectorInline",
    "scala.collection.immutable.VectorIterator",
    "scala.collection.immutable.VectorMap",
    "scala.collection.immutable.VectorMapBuilder",
    "scala.collection.immutable.VectorSliceBuilder",
    "scala.collection.immutable.VectorStatics",
    "scala.collection.immutable.VectorStepperBase",
    "scala.collection.immutable.WrappedString",
    "scala.collection.IndexedSeq",
    "scala.collection.IndexedSeqOps",
    "scala.collection.IndexedSeqView",
    "scala.collection.IntStepper",
    "scala.collection.Iterable",
    "scala.collection.IterableFactory",
    "scala.collection.IterableFactoryDefaults",
    "scala.collection.IterableOnce",
    "scala.collection.IterableOnceExtensionMethods",
    "scala.collection.IterableOnceOps",
    "scala.collection.IterableOps",
    "scala.collection.Iterator",
    "scala.collection.JavaConverters",
    "scala.collection.LazyZip2",
    "scala.collection.LazyZip3",
    "scala.collection.LazyZip4",
    "scala.collection.LinearSeq",
    "scala.collection.LinearSeqIterator",
    "scala.collection.LinearSeqOps",
    "scala.collection.LongStepper",
    "scala.collection.Map",
    "scala.collection.MapFactory",
    "scala.collection.MapFactoryDefaults",
    "scala.collection.MapOps",
    "scala.collection.MapView",
    "scala.collection.MapViewFactory",
    "scala.collection.mutable.AbstractBuffer",
    "scala.collection.mutable.AbstractIterable",
    "scala.collection.mutable.AbstractMap",
    "scala.collection.mutable.AbstractSeq",
    "scala.collection.mutable.AbstractSet",
    "scala.collection.mutable.AnyRefMap",
    "scala.collection.mutable.ArrayBuffer",
    "scala.collection.mutable.ArrayBufferView",
    "scala.collection.mutable.ArrayBuilder",
    "scala.collection.mutable.ArrayDeque",
    "scala.collection.mutable.ArrayDequeOps",
    "scala.collection.mutable.ArraySeq",
    "scala.collection.mutable.BitSet",
    "scala.collection.mutable.Buffer",
    "scala.collection.mutable.Builder",
    "scala.collection.mutable.Clearable",
    "scala.collection.mutable.Cloneable",
    "scala.collection.mutable.CollisionProofHashMap",
    "scala.collection.mutable.DoublingUnrolledBuffer",
    "scala.collection.mutable.Growable",
    "scala.collection.mutable.GrowableBuilder",
    "scala.collection.mutable.HashEntry",
    "scala.collection.mutable.HashMap",
    "scala.collection.mutable.HashSet",
    "scala.collection.mutable.HashTable",
    "scala.collection.mutable.ImmutableBuilder",
    "scala.collection.mutable.IndexedBuffer",
    "scala.collection.mutable.IndexedSeq",
    "scala.collection.mutable.IndexedSeqOps",
    "scala.collection.mutable.Iterable",
    "scala.collection.mutable.LinkedHashMap",
    "scala.collection.mutable.LinkedHashSet",
    "scala.collection.mutable.ListBuffer",
    "scala.collection.mutable.ListMap",
    "scala.collection.mutable.LongMap",
    "scala.collection.mutable.Map",
    "scala.collection.mutable.MapOps",
    "scala.collection.mutable.MultiMap",
    "scala.collection.mutable.OpenHashMap",
    "scala.collection.mutable.package",
    "scala.collection.mutable.PriorityQueue",
    "scala.collection.mutable.Queue",
    "scala.collection.mutable.RedBlackTree",
    "scala.collection.mutable.ReusableBuilder",
    "scala.collection.mutable.Seq",
    "scala.collection.mutable.SeqMap",
    "scala.collection.mutable.SeqOps",
    "scala.collection.mutable.Set",
    "scala.collection.mutable.SetOps",
    "scala.collection.mutable.Shrinkable",
    "scala.collection.mutable.SortedMap",
    "scala.collection.mutable.SortedMapOps",
    "scala.collection.mutable.SortedSet",
    "scala.collection.mutable.SortedSetOps",
    "scala.collection.mutable.Stack",
    "scala.collection.mutable.StringBuilder",
    "scala.collection.mutable.TreeMap",
    "scala.collection.mutable.TreeSet",
    "scala.collection.mutable.UnrolledBuffer",
    "scala.collection.mutable.WeakHashMap",
    "scala.collection.package",
    "scala.collection.Searching",
    "scala.collection.Seq",
    "scala.collection.SeqFactory",
    "scala.collection.SeqMap",
    "scala.collection.SeqOps",
    "scala.collection.SeqView",
    "scala.collection.Set",
    "scala.collection.SetOps",
    "scala.collection.SortedIterableFactory",
    "scala.collection.SortedMap",
    "scala.collection.SortedMapFactory",
    "scala.collection.SortedMapFactoryDefaults",
    "scala.collection.SortedMapOps",
    "scala.collection.SortedOps",
    "scala.collection.SortedSet",
    "scala.collection.SortedSetFactoryDefaults",
    "scala.collection.SortedSetOps",
    "scala.collection.SpecificIterableFactory",
    "scala.collection.Stepper",
    "scala.collection.StepperShape",
    "scala.collection.StepperShapeLowPriority1",
    "scala.collection.StepperShapeLowPriority2",
    "scala.collection.StrictOptimizedClassTagSeqFactory",
    "scala.collection.StrictOptimizedIterableOps",
    "scala.collection.StrictOptimizedLinearSeqOps",
    "scala.collection.StrictOptimizedMapOps",
    "scala.collection.StrictOptimizedSeqFactory",
    "scala.collection.StrictOptimizedSeqOps",
    "scala.collection.StrictOptimizedSetOps",
    "scala.collection.StrictOptimizedSortedMapOps",
    "scala.collection.StrictOptimizedSortedSetOps",
    "scala.collection.StringOps",
    "scala.collection.StringParsers",
    "scala.collection.StringView",
    "scala.collection.View",
    "scala.collection.WithFilter",
    "scala.compat.Platform",
    "scala.compiletime.ops.package",
    "scala.compiletime.package",
    "scala.compiletime.testing.Error",
    "scala.compiletime.testing.ErrorKind",
    "scala.compiletime.testing.package",
    "scala.concurrent.Await",
    "scala.concurrent.Awaitable",
    "scala.concurrent.AwaitPermission",
    "scala.concurrent.Batchable",
    "scala.concurrent.BatchingExecutor",
    "scala.concurrent.BatchingExecutorStatics",
    "scala.concurrent.BlockContext",
    "scala.concurrent.CanAwait",
    "scala.concurrent.Channel",
    "scala.concurrent.DelayedLazyVal",
    "scala.concurrent.duration.Deadline",
    "scala.concurrent.duration.Duration",
    "scala.concurrent.duration.DurationConversions",
    "scala.concurrent.duration.FiniteDuration",
    "scala.concurrent.duration.package",
    "scala.concurrent.ExecutionContext",
    "scala.concurrent.ExecutionContextExecutor",
    "scala.concurrent.ExecutionContextExecutorService",
    "scala.concurrent.Future",
    "scala.concurrent.impl.CompletionLatch",
    "scala.concurrent.impl.ExecutionContextImpl",
    "scala.concurrent.impl.FutureConvertersImpl",
    "scala.concurrent.impl.Promise",
    "scala.concurrent.JavaConversions",
    "scala.concurrent.OnCompleteRunnable",
    "scala.concurrent.package",
    "scala.concurrent.Promise",
    "scala.concurrent.SyncChannel",
    "scala.concurrent.SyncVar",
    "scala.Console",
    "scala.Conversion",
    "scala.DelayedInit",
    "scala.deprecated",
    "scala.deprecatedInheritance",
    "scala.deprecatedName",
    "scala.deprecatedOverriding",
    "scala.deriving",
    "scala.Double",
    "scala.DummyImplicit",
    "scala.Dynamic",
    "scala.Enum",
    "scala.Enumeration",
    "scala.Eql",
    "scala.Equals",
    "scala.Float",
    "scala.Function",
    "scala.Function0",
    "scala.Function1",
    "scala.Function10",
    "scala.Function11",
    "scala.Function12",
    "scala.Function13",
    "scala.Function14",
    "scala.Function15",
    "scala.Function16",
    "scala.Function17",
    "scala.Function18",
    "scala.Function19",
    "scala.Function2",
    "scala.Function20",
    "scala.Function21",
    "scala.Function22",
    "scala.Function3",
    "scala.Function4",
    "scala.Function5",
    "scala.Function6",
    "scala.Function7",
    "scala.Function8",
    "scala.Function9",
    "scala.FunctionXXL",
    "scala.IArray$package",
    "scala.implicits.package",
    "scala.inline",
    "scala.Int",
    "scala.internal.Chars",
    "scala.internal.MatchCase",
    "scala.internal.quoted.CompileTime",
    "scala.internal.quoted.Expr",
    "scala.internal.quoted.Matcher",
    "scala.internal.quoted.showName",
    "scala.internal.quoted.Type",
    "scala.internal.quoted.Unpickler",
    "scala.internal.tasty.CompilerInterface",
    "scala.internal.TupledFunction",
    "scala.internal.TypeBox",
    "scala.io.AnsiColor",
    "scala.io.BufferedSource",
    "scala.io.Codec",
    "scala.io.LowPriorityCodecImplicits",
    "scala.io.Position",
    "scala.io.Source",
    "scala.io.StdIn",
    "scala.jdk.Accumulator",
    "scala.jdk.AnyAccumulator",
    "scala.jdk.AnyAccumulatorStepper",
    "scala.jdk.CollectionConverters",
    "scala.jdk.DoubleAccumulator",
    "scala.jdk.DoubleAccumulatorStepper",
    "scala.jdk.DurationConverters",
    "scala.jdk.FunctionConverters",
    "scala.jdk.FunctionWrappers",
    "scala.jdk.FutureConverters",
    "scala.jdk.IntAccumulator",
    "scala.jdk.IntAccumulatorStepper",
    "scala.jdk.javaapi.CollectionConverters",
    "scala.jdk.javaapi.DurationConverters",
    "scala.jdk.javaapi.FunctionConverters",
    "scala.jdk.javaapi.FutureConverters",
    "scala.jdk.javaapi.OptionConverters",
    "scala.jdk.javaapi.StreamConverters",
    "scala.jdk.LongAccumulator",
    "scala.jdk.LongAccumulatorStepper",
    "scala.jdk.OptionConverters",
    "scala.jdk.OptionShape",
    "scala.jdk.Priority0FunctionExtensions",
    "scala.jdk.Priority1FunctionExtensions",
    "scala.jdk.Priority2FunctionExtensions",
    "scala.jdk.Priority3FunctionExtensions",
    "scala.jdk.StreamConverters",
    "scala.languageFeature",
    "scala.Long",
    "scala.LowPriorityImplicits",
    "scala.LowPriorityImplicits2",
    "scala.main",
    "scala.MatchError",
    "scala.math.BigDecimal",
    "scala.math.BigInt",
    "scala.math.Equiv",
    "scala.math.Fractional",
    "scala.math.Integral",
    "scala.math.LowPriorityEquiv",
    "scala.math.LowPriorityOrderingImplicits",
    "scala.math.Numeric",
    "scala.math.Ordered",
    "scala.math.Ordering",
    "scala.math.package",
    "scala.math.PartiallyOrdered",
    "scala.math.PartialOrdering",
    "scala.math.ScalaNumericAnyConversions",
    "scala.math.ScalaNumericConversions",
    "scala.native",
    "scala.noinline",
    "scala.None",
    "scala.NonEmptyTuple",
    "scala.NotImplementedError",
    "scala.opaques",
    "scala.Option",
    "scala.package",
    "scala.PartialFunction",
    "scala.PolyFunction",
    "scala.Predef",
    "scala.Product",
    "scala.Product0",
    "scala.Product1",
    "scala.Product10",
    "scala.Product11",
    "scala.Product12",
    "scala.Product13",
    "scala.Product14",
    "scala.Product15",
    "scala.Product16",
    "scala.Product17",
    "scala.Product18",
    "scala.Product19",
    "scala.Product2",
    "scala.Product20",
    "scala.Product21",
    "scala.Product22",
    "scala.Product3",
    "scala.Product4",
    "scala.Product5",
    "scala.Product6",
    "scala.Product7",
    "scala.Product8",
    "scala.Product9",
    "scala.Proxy",
    "scala.quoted.Const",
    "scala.quoted.Consts",
    "scala.quoted.Expr",
    "scala.quoted.Lambda",
    "scala.quoted.Liftable",
    "scala.quoted.qctx$package",
    "scala.quoted.QuoteContext",
    "scala.quoted.report",
    "scala.quoted.ScopeException",
    "scala.quoted.show.SyntaxHighlight",
    "scala.quoted.Type",
    "scala.quoted.Unliftable",
    "scala.quoted.Unlifted",
    "scala.quoted.unsafe.UnsafeExpr",
    "scala.quoted.util.ExprMap",
    "scala.quoted.util.Var",
    "scala.quoted.Varargs",
    "scala.ref.PhantomReference",
    "scala.ref.PhantomReferenceWithWrapper",
    "scala.ref.Reference",
    "scala.ref.ReferenceQueue",
    "scala.ref.ReferenceWithWrapper",
    "scala.ref.ReferenceWrapper",
    "scala.ref.SoftReference",
    "scala.ref.SoftReferenceWithWrapper",
    "scala.ref.WeakReference",
    "scala.ref.WeakReferenceWithWrapper",
    "scala.reflect.AnyValManifest",
    "scala.reflect.ClassManifestDeprecatedApis",
    "scala.reflect.ClassManifestFactory",
    "scala.reflect.ClassTag",
    "scala.reflect.ClassTypeManifest",
    "scala.reflect.macros.internal.macroImpl",
    "scala.reflect.Manifest",
    "scala.reflect.ManifestFactory",
    "scala.reflect.NameTransformer",
    "scala.reflect.NoManifest",
    "scala.reflect.OptManifest",
    "scala.reflect.package",
    "scala.reflect.Selectable",
    "scala.runtime.AbstractFunction0",
    "scala.runtime.AbstractFunction1",
    "scala.runtime.AbstractFunction10",
    "scala.runtime.AbstractFunction11",
    "scala.runtime.AbstractFunction12",
    "scala.runtime.AbstractFunction13",
    "scala.runtime.AbstractFunction14",
    "scala.runtime.AbstractFunction15",
    "scala.runtime.AbstractFunction16",
    "scala.runtime.AbstractFunction17",
    "scala.runtime.AbstractFunction18",
    "scala.runtime.AbstractFunction19",
    "scala.runtime.AbstractFunction2",
    "scala.runtime.AbstractFunction20",
    "scala.runtime.AbstractFunction21",
    "scala.runtime.AbstractFunction22",
    "scala.runtime.AbstractFunction3",
    "scala.runtime.AbstractFunction4",
    "scala.runtime.AbstractFunction5",
    "scala.runtime.AbstractFunction6",
    "scala.runtime.AbstractFunction7",
    "scala.runtime.AbstractFunction8",
    "scala.runtime.AbstractFunction9",
    "scala.runtime.AbstractPartialFunction",
    "scala.runtime.ArrayCharSequence",
    "scala.runtime.EmptyMethodCache",
    "scala.runtime.EnumValue",
    "scala.runtime.FractionalProxy",
    "scala.runtime.IntegralProxy",
    "scala.runtime.java8.JFunction0$mcB$sp",
    "scala.runtime.java8.JFunction0$mcC$sp",
    "scala.runtime.java8.JFunction0$mcD$sp",
    "scala.runtime.java8.JFunction0$mcF$sp",
    "scala.runtime.java8.JFunction0$mcI$sp",
    "scala.runtime.java8.JFunction0$mcJ$sp",
    "scala.runtime.java8.JFunction0$mcS$sp",
    "scala.runtime.java8.JFunction0$mcV$sp",
    "scala.runtime.java8.JFunction0$mcZ$sp",
    "scala.runtime.java8.JFunction1$mcDD$sp",
    "scala.runtime.java8.JFunction1$mcDF$sp",
    "scala.runtime.java8.JFunction1$mcDI$sp",
    "scala.runtime.java8.JFunction1$mcDJ$sp",
    "scala.runtime.java8.JFunction1$mcFD$sp",
    "scala.runtime.java8.JFunction1$mcFF$sp",
    "scala.runtime.java8.JFunction1$mcFI$sp",
    "scala.runtime.java8.JFunction1$mcFJ$sp",
    "scala.runtime.java8.JFunction1$mcID$sp",
    "scala.runtime.java8.JFunction1$mcIF$sp",
    "scala.runtime.java8.JFunction1$mcII$sp",
    "scala.runtime.java8.JFunction1$mcIJ$sp",
    "scala.runtime.java8.JFunction1$mcJD$sp",
    "scala.runtime.java8.JFunction1$mcJF$sp",
    "scala.runtime.java8.JFunction1$mcJI$sp",
    "scala.runtime.java8.JFunction1$mcJJ$sp",
    "scala.runtime.java8.JFunction1$mcVD$sp",
    "scala.runtime.java8.JFunction1$mcVF$sp",
    "scala.runtime.java8.JFunction1$mcVI$sp",
    "scala.runtime.java8.JFunction1$mcVJ$sp",
    "scala.runtime.java8.JFunction1$mcZD$sp",
    "scala.runtime.java8.JFunction1$mcZF$sp",
    "scala.runtime.java8.JFunction1$mcZI$sp",
    "scala.runtime.java8.JFunction1$mcZJ$sp",
    "scala.runtime.java8.JFunction2$mcDDD$sp",
    "scala.runtime.java8.JFunction2$mcDDI$sp",
    "scala.runtime.java8.JFunction2$mcDDJ$sp",
    "scala.runtime.java8.JFunction2$mcDID$sp",
    "scala.runtime.java8.JFunction2$mcDII$sp",
    "scala.runtime.java8.JFunction2$mcDIJ$sp",
    "scala.runtime.java8.JFunction2$mcDJD$sp",
    "scala.runtime.java8.JFunction2$mcDJI$sp",
    "scala.runtime.java8.JFunction2$mcDJJ$sp",
    "scala.runtime.java8.JFunction2$mcFDD$sp",
    "scala.runtime.java8.JFunction2$mcFDI$sp",
    "scala.runtime.java8.JFunction2$mcFDJ$sp",
    "scala.runtime.java8.JFunction2$mcFID$sp",
    "scala.runtime.java8.JFunction2$mcFII$sp",
    "scala.runtime.java8.JFunction2$mcFIJ$sp",
    "scala.runtime.java8.JFunction2$mcFJD$sp",
    "scala.runtime.java8.JFunction2$mcFJI$sp",
    "scala.runtime.java8.JFunction2$mcFJJ$sp",
    "scala.runtime.java8.JFunction2$mcIDD$sp",
    "scala.runtime.java8.JFunction2$mcIDI$sp",
    "scala.runtime.java8.JFunction2$mcIDJ$sp",
    "scala.runtime.java8.JFunction2$mcIID$sp",
    "scala.runtime.java8.JFunction2$mcIII$sp",
    "scala.runtime.java8.JFunction2$mcIIJ$sp",
    "scala.runtime.java8.JFunction2$mcIJD$sp",
    "scala.runtime.java8.JFunction2$mcIJI$sp",
    "scala.runtime.java8.JFunction2$mcIJJ$sp",
    "scala.runtime.java8.JFunction2$mcJDD$sp",
    "scala.runtime.java8.JFunction2$mcJDI$sp",
    "scala.runtime.java8.JFunction2$mcJDJ$sp",
    "scala.runtime.java8.JFunction2$mcJID$sp",
    "scala.runtime.java8.JFunction2$mcJII$sp",
    "scala.runtime.java8.JFunction2$mcJIJ$sp",
    "scala.runtime.java8.JFunction2$mcJJD$sp",
    "scala.runtime.java8.JFunction2$mcJJI$sp",
    "scala.runtime.java8.JFunction2$mcJJJ$sp",
    "scala.runtime.java8.JFunction2$mcVDD$sp",
    "scala.runtime.java8.JFunction2$mcVDI$sp",
    "scala.runtime.java8.JFunction2$mcVDJ$sp",
    "scala.runtime.java8.JFunction2$mcVID$sp",
    "scala.runtime.java8.JFunction2$mcVII$sp",
    "scala.runtime.java8.JFunction2$mcVIJ$sp",
    "scala.runtime.java8.JFunction2$mcVJD$sp",
    "scala.runtime.java8.JFunction2$mcVJI$sp",
    "scala.runtime.java8.JFunction2$mcVJJ$sp",
    "scala.runtime.java8.JFunction2$mcZDD$sp",
    "scala.runtime.java8.JFunction2$mcZDI$sp",
    "scala.runtime.java8.JFunction2$mcZDJ$sp",
    "scala.runtime.java8.JFunction2$mcZID$sp",
    "scala.runtime.java8.JFunction2$mcZII$sp",
    "scala.runtime.java8.JFunction2$mcZIJ$sp",
    "scala.runtime.java8.JFunction2$mcZJD$sp",
    "scala.runtime.java8.JFunction2$mcZJI$sp",
    "scala.runtime.java8.JFunction2$mcZJJ$sp",
    "scala.runtime.LambdaDeserialize",
    "scala.runtime.LambdaDeserializer",
    "scala.runtime.LazyBoolean",
    "scala.runtime.LazyByte",
    "scala.runtime.LazyChar",
    "scala.runtime.LazyDouble",
    "scala.runtime.LazyFloat",
    "scala.runtime.LazyInt",
    "scala.runtime.LazyLong",
    "scala.runtime.LazyRef",
    "scala.runtime.LazyShort",
    "scala.runtime.LazyUnit",
    "scala.runtime.MegaMethodCache",
    "scala.runtime.MethodCache",
    "scala.runtime.NonLocalReturnControl",
    "scala.runtime.Nothing$",
    "scala.runtime.Null$",
    "scala.runtime.OrderedProxy",
    "scala.runtime.PolyMethodCache",
    "scala.runtime.RangedProxy",
    "scala.runtime.RichBoolean",
    "scala.runtime.RichByte",
    "scala.runtime.RichChar",
    "scala.runtime.RichDouble",
    "scala.runtime.RichFloat",
    "scala.runtime.RichInt",
    "scala.runtime.RichLong",
    "scala.runtime.RichShort",
    "scala.runtime.ScalaNumberProxy",
    "scala.runtime.ScalaRunTime",
    "scala.runtime.ScalaWholeNumberProxy",
    "scala.runtime.StructuralCallSite",
    "scala.runtime.Tuple",
    "scala.runtime.Tuple2Zipped",
    "scala.runtime.Tuple3Zipped",
    "scala.runtime.TupleXXL",
    "scala.runtime.ZippedIterable2",
    "scala.runtime.ZippedIterable3",
    "scala.ScalaReflectionException",
    "scala.Selectable",
    "scala.SerialVersionUID",
    "scala.Short",
    "scala.Some",
    "scala.Specializable",
    "scala.specialized",
    "scala.StringContext",
    "scala.Symbol",
    "scala.sys.BooleanProp",
    "scala.sys.CreatorImpl",
    "scala.sys.package",
    "scala.sys.process.BasicIO",
    "scala.sys.process.FileProcessLogger",
    "scala.sys.process.package",
    "scala.sys.process.Parser",
    "scala.sys.process.Process",
    "scala.sys.process.ProcessBuilder",
    "scala.sys.process.ProcessBuilderImpl",
    "scala.sys.process.ProcessCreation",
    "scala.sys.process.ProcessImpl",
    "scala.sys.process.ProcessImplicits",
    "scala.sys.process.processInternal",
    "scala.sys.process.ProcessIO",
    "scala.sys.process.ProcessLogger",
    "scala.sys.Prop",
    "scala.sys.PropImpl",
    "scala.sys.ShutdownHookThread",
    "scala.sys.SystemProperties",
    "scala.tasty.reflect.ExprCastError",
    "scala.tasty.reflect.ExtractorsPrinter",
    "scala.tasty.reflect.Printer",
    "scala.tasty.reflect.SourceCodePrinter",
    "scala.tasty.reflect.TreeAccumulator",
    "scala.tasty.reflect.TreeMap",
    "scala.tasty.reflect.TreeTraverser",
    "scala.tasty.reflect.Types",
    "scala.tasty.reflect.TypeTest$package",
    "scala.tasty.Reflection",
    "scala.throws",
    "scala.transient",
    "scala.Tuple",
    "scala.Tuple$package",
    "scala.Tuple1",
    "scala.Tuple10",
    "scala.Tuple11",
    "scala.Tuple12",
    "scala.Tuple13",
    "scala.Tuple14",
    "scala.Tuple15",
    "scala.Tuple16",
    "scala.Tuple17",
    "scala.Tuple18",
    "scala.Tuple19",
    "scala.Tuple2",
    "scala.Tuple20",
    "scala.Tuple21",
    "scala.Tuple22",
    "scala.Tuple3",
    "scala.Tuple4",
    "scala.Tuple5",
    "scala.Tuple6",
    "scala.Tuple7",
    "scala.Tuple8",
    "scala.Tuple9",
    "scala.TupledFunction",
    "scala.unchecked",
    "scala.UninitializedError",
    "scala.UninitializedFieldError",
    "scala.UniquenessCache",
    "scala.Unit",
    "scala.util.ChainingOps",
    "scala.util.ChainingSyntax",
    "scala.util.CommandLineParser",
    "scala.util.control.BreakControl",
    "scala.util.control.Breaks",
    "scala.util.control.ControlThrowable",
    "scala.util.control.Exception",
    "scala.util.control.NonFatal",
    "scala.util.control.NonLocalReturns",
    "scala.util.control.NoStackTrace",
    "scala.util.control.TailCalls",
    "scala.util.DynamicVariable",
    "scala.util.Either",
    "scala.util.Failure",
    "scala.util.FromDigits",
    "scala.util.FromString",
    "scala.util.hashing.ByteswapHashing",
    "scala.util.hashing.Hashing",
    "scala.util.hashing.MurmurHash3",
    "scala.util.hashing.package",
    "scala.util.Left",
    "scala.util.LowPriorityNot",
    "scala.util.matching.Regex",
    "scala.util.matching.UnanchoredRegex",
    "scala.util.Not",
    "scala.util.package",
    "scala.util.Properties",
    "scala.util.PropertiesTrait",
    "scala.util.Random",
    "scala.util.Right",
    "scala.util.Sorting",
    "scala.util.Success",
    "scala.util.Try",
    "scala.util.Using",
    "scala.ValueOf",
    "scala.volatile",
    "scalaShadowing.language",
  )

  val blacklist = Set[String](
    "dotty.internal.CompileTimeMacros",
    "dotty.runtime.LazyVals",
    "scala.collection.ArrayOps",
    "scala.collection.BitSet",
    "scala.collection.BitSetOps",
    "scala.collection.generic.DefaultSerializationProxy",
    "scala.collection.immutable.ChampCommon",
    "scala.collection.immutable.LazyList",
    "scala.collection.immutable.List",
    "scala.collection.immutable.Node",
    "scala.collection.immutable.NumericRange",
    "scala.collection.immutable.Range",
    "scala.collection.immutable.StrictOptimizedSeqOps",
    "scala.collection.immutable.TreeSeqMap",
    "scala.collection.immutable.Vector",
    "scala.collection.immutable.Vector0",
    "scala.collection.immutable.VectorBuilder",
    "scala.collection.immutable.VectorImpl",
    "scala.collection.immutable.VectorSliceBuilder",
    "scala.collection.immutable.WrappedString",
    "scala.collection.LazyZip2",
    "scala.collection.LazyZip3",
    "scala.collection.LazyZip4",
    "scala.collection.LazyZipOps",
    "scala.collection.Map",
    "scala.collection.MapOps",
    "scala.collection.mutable.ArrayBuffer",
    "scala.collection.mutable.ArrayBufferView",
    "scala.collection.mutable.ArrayDeque",
    "scala.collection.mutable.ArrayDequeOps",
    "scala.collection.mutable.CollisionProofHashMap",
    "scala.collection.mutable.HashMap",
    "scala.collection.mutable.HashSet",
    "scala.collection.mutable.ListBuffer",
    "scala.collection.mutable.UnrolledBuffer",
    "scala.collection.SeqView",
    "scala.collection.StringOps",
    "scala.collection.StringView",
    "scala.concurrent.duration.Duration",
    "scala.concurrent.Future",
    "scala.concurrent.Future",
    "scala.Enumeration",
    "scala.Enumeration",
    "scala.internal.quoted.Matcher",
    "scala.io.Source",
    "scala.io.Source",
    "scala.jdk.Accumulator",
    "scala.jdk.DoubleAccumulator",
    "scala.jdk.IntAccumulator",
    "scala.jdk.javaapi.DurationConverters",
    "scala.jdk.LongAccumulator",
    "scala.Product",
    "scala.Product1",
    "scala.Product10",
    "scala.Product11",
    "scala.Product12",
    "scala.Product13",
    "scala.Product14",
    "scala.Product15",
    "scala.Product16",
    "scala.Product17",
    "scala.Product18",
    "scala.Product19",
    "scala.Product2",
    "scala.Product20",
    "scala.Product21",
    "scala.Product22",
    "scala.Product3",
    "scala.Product4",
    "scala.Product5",
    "scala.Product6",
    "scala.Product7",
    "scala.Product8",
    "scala.Product9",
    "scala.quoted.Expr",
    "scala.reflect.ClassTag",
    "scala.runtime.ArrayCharSequence",
    "scala.runtime.LazyBoolean",
    "scala.runtime.LazyByte",
    "scala.runtime.LazyChar",
    "scala.runtime.LazyDouble",
    "scala.runtime.LazyFloat",
    "scala.runtime.LazyInt",
    "scala.runtime.LazyLong",
    "scala.runtime.LazyRef",
    "scala.runtime.LazyRef",
    "scala.runtime.LazyShort",
    "scala.runtime.LazyUnit",
    "scala.runtime.Tuple2Zipped",
    "scala.runtime.Tuple3Zipped",
    "scala.StringContext",
    "scala.sys.process.ProcessImpl",
    "scala.tasty.reflect.ExtractorsPrinter",
    "scala.tasty.reflect.SourceCodePrinter",
    "scala.tasty.reflect.Types",
    "scala.util.CommandLineParser",
    "scala.util.CommandLineParser",
    "scala.util.FromDigits",
    "scala.util.Properties",
    "scala.util.PropertiesTrait",
  )

  inspector.inspect(
    "/Users/nicolasstucki/GitHub/dotty/community-build/community-projects/stdLib213/build/pack/lib/scala-library.jar",
    classes.filterNot(blacklist)
  )
}
