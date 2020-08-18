import React, { useCallback } from "react";
import * as Yup from "yup";
import {
  MarkdownEditor as _MarkdownEditor,
  Box,
  Button,
  Input,
  ErrorMessage,
} from "@tlon/indigo-react";
import { dateToDa, stringToSymbol } from "../../../../lib/util";
import { AsyncButton } from '../../../../components/AsyncButton';

import { Formik, Form, FormikHelpers } from "formik";
import GlobalApi from "../../../../api/global";
import { useWaitForProps } from "../../../../lib/useWaitForProps";
import { Notebooks, Notebook } from "../../../../types/publish-update";
import { MarkdownField } from './MarkdownField';
import { RouteComponentProps } from "react-router-dom";

interface FormSchema {
  title: string;
  body: string;
}

const formSchema = Yup.object({
  title: Yup.string().required("Title cannot be blank"),
  body: Yup.string().required("Post cannot be blank"),
});

interface NewPostProps {
  api: GlobalApi;
  book: string;
  ship: string;
  notebook: Notebook;
}

function NewPost(props: NewPostProps & RouteComponentProps) {
  const { api, book, notebook, ship, history } = props;

  const waiter = useWaitForProps(props, 20000);

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      const noteId = stringToSymbol(values.title);
      const newNote = {
        "new-note": {
          who: window.ship,
          book: book,
          note: noteId,
          title: values.title,
          body: values.body,
        },
      };

      try {
        try {
          await api.publish.publishAction(newNote);
        } catch (e) {
          if (e.includes("note already exists")) {
            const timestamp = Math.floor(Date.now() / 1000);
            newNote["new-note"].note += "-" + timestamp;
            api.publish.publishAction(newNote);
          } else {
            throw e;
          }
        }

        await waiter((p) => {
          return !!p?.notebook?.notes[newNote["new-note"].note];
        });
        history.push(
          `/~publish/notebook/${ship}/${book}/note/${newNote["new-note"].note}`
        );
      } catch (e) {
        console.error(e);
        actions.setStatus({ error: "Posting note failed" });
      }
    },
    [api, waiter, book]
  );

  const initialValues: FormSchema = {
    title: "",
    body: "",
  };

  const date = dateToDa(new Date()).slice(1, -10);

  return (
    <Box
      width="100%"
      px={4}
      pb={4}
      display="grid"
      justifyItems="start"
      gridTemplateRows="64px 1fr"
      gridTemplateColumns="1fr 1fr"
      gridRowGap={2}
    >
      <Formik
        validationSchema={formSchema}
        initialValues={initialValues}
        onSubmit={onSubmit}
      >
        {({ isSubmitting, isValid }) => (
          <Form style={{ display: "contents" }}>
            <Input width="100%" placeholder="Post Title" id="title" />
            <Box mt={1} justifySelf="end">
              <AsyncButton
                loadingText="Posting..."
                disabled={!isValid}
                type="submit"
              >
                Publish To {notebook.title}
              </AsyncButton>
            </Box>
            <MarkdownField gridColumn="1/3" id="body" />
          </Form>
        )}
      </Formik>
    </Box>
  );
}

export default NewPost;
