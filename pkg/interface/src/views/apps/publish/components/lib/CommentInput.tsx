import React from "react";
import * as Yup from "yup";
import { Formik, FormikHelpers, Form } from "formik";
import { AsyncButton } from '../../../../components/AsyncButton';
import { TextArea } from "@tlon/indigo-react";

interface FormSchema {
  comment: string;
}

const formSchema = Yup.object({
  comment: Yup.string().required("Comment can't be empty"),
});

interface CommentInputProps {
  onSubmit: (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => Promise<void>;
  initial?: string;
  loadingText?: string;
  label?: string;
  placeholder?: string;
}

export default function CommentInput(props: CommentInputProps) {
  const initialValues: FormSchema = { comment: props.initial || "" };
  const label = props.label || "Add Comment";
  const loading = props.loadingText || "Commenting...";
  return (
    <Formik
      validationSchema={formSchema}
      onSubmit={props.onSubmit}
      initialValues={initialValues}
    >
      <Form>
        <TextArea id="comment" placeholder={props.placeholder || ""} />
        <AsyncButton loadingText={loading} border type="submit">
          {label}
        </AsyncButton>
      </Form>
    </Formik>
  );
}
