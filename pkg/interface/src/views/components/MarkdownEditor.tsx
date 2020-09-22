import React, { useMemo, useCallback } from "react";

import {
  schema,
  defaultMarkdownParser,
  defaultMarkdownSerializer,
} from "prosemirror-markdown";
import { exampleSetup } from "prosemirror-example-setup";
import "prosemirror-view/style/prosemirror.css";
import "prosemirror-menu/style/menu.css";
import { EditorState } from "prosemirror-state";
import { useProseMirror, ProseMirror } from "use-prosemirror";
import _ from "lodash";
import "~/views/apps/publish/css/ProseMirror.css"

interface MarkdownEditorProps {
  content: string;
  onChange: (s: string) => void;
}

export function MarkdownEditor(props: MarkdownEditorProps) {
  const [state, setState] = useProseMirror({
    schema,
    doc: defaultMarkdownParser.parse(props.content),
    plugins: exampleSetup({ schema, menuBar: true }),
  });

  const reportChange = _.debounce(
    useCallback(
      (doc: any) => {
        const content = defaultMarkdownSerializer.serialize(doc);
        props.onChange(content);
      },
      [props.onChange]
    ),
    500
  );

  const onChange = useCallback(
    (state: EditorState) => {
      reportChange(state.doc);
      setState(state);
    },
    [reportChange, setState]
  );

  return (
    <ProseMirror className="h-100 w-100" onChange={onChange} state={state} />
  );
}
