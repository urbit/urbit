import React from "react";
import * as Yup from "yup";
import { Box, Input } from "@tlon/indigo-react";
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
    <Box
      width="100%"
      height="100%"
      p={[2, 4]}
      display="grid"
      justifyItems="start"
      gridTemplateRows={["64px 64px 1fr", "64px 1fr"]}
      gridTemplateColumns={["100%", "1fr 1fr"]}
      gridColumnGap={2}
      gridRowGap={2}
    >
      <Formik
        validationSchema={formSchema}
        initialValues={initial}
        onSubmit={onSubmit}
      >
        <Form style={{ display: "contents" }}>
          <Input width="100%" placeholder="Post Title" id="title" />
          <Box gridRow={["1/2", "auto"]} mt={1} justifySelf={["start", "end"]}>
            <AsyncButton primary loadingText={loadingText}>
              {submitLabel}
            </AsyncButton>
          </Box>
          <MarkdownField gridColumn={["1/2", "1/3"]} id="body" />
        </Form>
      </Formik>
    </Box>
  );
}
