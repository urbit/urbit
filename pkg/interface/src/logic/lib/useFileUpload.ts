import _ from 'lodash';
import { useState, ClipboardEvent } from 'react';
import { useFileDrag } from './useDrag';
import useStorage, { IuseStorage } from './useStorage';

export type FileUploadSource = 'drag' | 'paste' | 'direct';

interface FileUploadEventHandlers {
  onSuccess: (url: string, source: FileUploadSource) => void;
  onError?: (error: Error) => void;
}

interface FileUploadHandler {
  onFiles: (
    files: FileList | File[],
    storage?: IuseStorage,
    uploadSource?: FileUploadSource
  ) => void | Promise<void>;
}

function isFileUploadHandler(obj: any): obj is FileUploadHandler {
  return typeof obj.onFiles === 'function';
}

type useFileUploadParams = {
  multiple?: boolean;
} & (FileUploadEventHandlers | FileUploadHandler)

export function useFileUpload({ multiple = true, ...params }: useFileUploadParams) {
  const storage = useStorage();
  const {
    canUpload, uploadDefault
  } = storage;
  const [source, setSource] = useState<FileUploadSource>('paste');
  const drag = useFileDrag(f => uploadFiles(f, 'drag'));

  function onPaste(event: ClipboardEvent) {
    if (!event.clipboardData || !event.clipboardData.files.length) {
      return;
    }

    event.preventDefault();
    event.stopPropagation();

    uploadFiles(event.clipboardData.files, 'paste');
  }

  function uploadFiles(files: FileList | File[], uploadSource: FileUploadSource) {
    if (isFileUploadHandler(params)) {
      return params.onFiles(files, storage, uploadSource);
    }

    if (!canUpload) {
      return;
    }

    setSource(uploadSource);

    const { onSuccess, onError } = params as FileUploadEventHandlers;
    const fileArray = Array.from(files);
    const toUpload = multiple ? fileArray : _.take(fileArray);
    toUpload.forEach((file) => {
      uploadDefault(file)
        .then(url => onSuccess(url, source))
        .catch((err: Error) => {
          console.log(err);
          onError && onError(err);
        });
    });
  }

  return {
    ...storage,
    onPaste,
    drag
  };
}
