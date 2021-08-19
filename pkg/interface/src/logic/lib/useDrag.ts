import { useCallback, useEffect, useState } from 'react';

function validateDragEvent(e: DragEvent): FileList | File[] | true | null {
  const files: File[] = [];
  let valid = false;
  if (e.dataTransfer?.files) {
    Array.from(e.dataTransfer.files).forEach(f => files.push(f));
  }
  if (e.dataTransfer?.items) {
    Array.from(e.dataTransfer.items || [])
      .filter(i => i.kind === 'file')
      .forEach((f) => {
        valid = true; // Valid if file exists, but on DragOver, won't reveal its contents for security
        const data = f.getAsFile();
        if (data) {
          files.push(data);
        }
      });
  }
  if (files.length) {
    return [...new Set(files)];
  }
  if (navigator.userAgent.includes('Safari')) {
    if (e.dataTransfer?.effectAllowed === 'all') {
      valid = true;
    } else if (e.dataTransfer?.files.length) {
      return e.dataTransfer.files;
    }
  }
  if (valid) {
    return true;
  }
  return null;
}

export function useFileDrag(dragged: (f: FileList | File[], e: DragEvent) => void) {
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
      const files = validateDragEvent(e);
      if (!files || files === true) {
        return;
      }
      e.preventDefault();
      dragged(files, e);
    },
    [setDragging, dragged]
  );

  const onDragOver = useCallback(
    (e: DragEvent) => {
      if (!validateDragEvent(e)) {
        return;
      }
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

  useEffect(() => {
    const mouseleave = (e) => {
      if (!e.relatedTarget && !e.toElement) {
        setDragging(false);
      }
    };
    document.body.addEventListener('mouseout', mouseleave);
    return () => {
      document.body.removeEventListener('mouseout', mouseleave);
    };
  }, []);

  const bind = {
    onDragLeave,
    onDragOver,
    onDrop,
    onDragEnter
  };

  return { bind, dragging };
}
