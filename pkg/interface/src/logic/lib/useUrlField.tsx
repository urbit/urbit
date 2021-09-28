import { useField } from 'formik';
import { MutableRefObject, useCallback, useMemo } from 'react';
import useStorage from './useStorage';

export function useUrlField(
  id: string,
  ref: MutableRefObject<HTMLInputElement>
) {
  const [field, meta, helpers] = useField(id);
  const { setValue, setError } = helpers;

  const storage = useStorage();
  const { uploadDefault, canUpload } = storage;

  const onImageUpload = useCallback(async () => {
    const file = ref.current?.files?.item(0);

    if (!file || !canUpload) {
      return;
    }
    try {
      const url = await uploadDefault(file);
      setValue(url);
    } catch (e) {
      setError(e.message);
    }
  }, [ref.current, uploadDefault, canUpload, setValue]);
  const extStorage = useMemo(() => ({ ...storage, onImageUpload }), [
    storage,
    onImageUpload
  ]);

  return [field, meta, helpers, extStorage] as const;
}
