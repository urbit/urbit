import { useState, useCallback, useMemo, useEffect } from "react";

function validateDragEvent(e: DragEvent): FileList | null {
  const files = e.dataTransfer?.files;
  console.log(files);
  if(!files?.length) {
    return null;
  }
  return files || null;
}

export function useFileDrag(dragged: (f: FileList) => void) {
  const [dragging, setDragging] = useState(false);

  const onDragEnter = useCallback(
    (e: DragEvent) => {
      if (!validateDragEvent(e)) {
        return;
      }
      setDragging(true);
    },
    [setDragging]
  );

  const onDrop = useCallback(
    (e: DragEvent) => {
      setDragging(false);
      e.preventDefault();
      const files = validateDragEvent(e);
      if (!files) {
        return;
      }
      dragged(files);
    },
    [setDragging, dragged]
  );

  const onDragOver = useCallback(
    (e: DragEvent) => {
      e.preventDefault();
      setDragging(true);
    },
    [setDragging]
  );

  const onDragLeave = useCallback(
    (e: DragEvent) => {
      const over = document.elementFromPoint(e.clientX, e.clientY);
      if (!over || !(e.currentTarget as any)?.contains(over)) {
        setDragging(false);
      }
    },
    [setDragging]
  );

  const bind = {
    onDragLeave,
    onDragOver,
    onDrop,
    onDragEnter,
  };

  return { bind, dragging };
}
