import React from "react";
import * as Yup from "yup";
import {
  Box,
  ManagedTextInputField as Input,
  TwoUp,
  Col,
} from "@tlon/indigo-react";
import { AsyncButton } from "../../../../components/AsyncButton";
import { Formik, Form, FormikHelpers } from "formik";
import { MarkdownField } from "./MarkdownField";

interface PostFormProps {
  initial: PostFormSchema;
  onSubmit: (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => Promise<any>;
  submitLabel: string;
  loadingText: string;
}

const formSchema = Yup.object({
  title: Yup.string().required("Title cannot be blank"),
  body: Yup.string().required("Post cannot be blank"),
});

export interface PostFormSchema {
  title: string;
  body: string;
}

export function PostForm(props: PostFormProps) {
  const { initial, onSubmit, submitLabel, loadingText } = props;

  return (
    <Col width="100%" height="100%" p={[2, 4]}>
      <Formik
        validationSchema={formSchema}
        initialValues={initial}
        onSubmit={onSubmit}
        validateOnBlur
      >
        <Form style={{ display: "contents" }}>
          <TwoUp mb={4} gap={4}>
            <Input
              flexGrow={1}
              placeholder="Post Title"
              id="title"
            />
            <AsyncButton flexShrink={1} primary loadingText={loadingText}>
              {submitLabel}
            </AsyncButton>
          </TwoUp>
          <MarkdownField flexGrow={1} id="body" />
        </Form>
      </Formik>
    </Col>
  );
}
