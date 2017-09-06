--- lib/ext2.c.orig
+++ lib/ext2.c
@@ -139,8 +139,10 @@
 	if(! (flags & ~EXT2_NOCOMPR_FL) )
 		x_addprop(f, "NoCompBlock");
 #endif
+#ifdef EXT2_ECOMPR_FL
 	if(! (flags & ~EXT2_ECOMPR_FL) )
 		x_addprop(f, "CompError");
+#endif
 	if(! (flags & ~EXT2_BTREE_FL) )
 		x_addprop(f, "BTree");
 	if(! (flags & ~EXT2_INDEX_FL) )
@@ -225,8 +227,10 @@
 	if( e2prop_get(f, "NoCompBlock", (char **)&tmp) == 0 )
 		flags |= EXT2_NOCOMPR_FL ;
 #endif
+#ifdef EXT2_ECOMPR_FL
 	if( e2prop_get(f, "CompError", (char **)&tmp) == 0 )
 		flags |= EXT2_ECOMPR_FL ;
+#endif
 	if( e2prop_get(f, "BTree", (char **)&tmp) == 0 )
 		flags |= EXT2_BTREE_FL ;
 	if( e2prop_get(f, "HashIndexed", (char **)&tmp) == 0 )
