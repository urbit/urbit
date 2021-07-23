import React from 'react';
import * as Yup from 'yup';
import {
  ManagedTextInputField as Input,
  Row,
  Col,
  Button
} from '@tlon/indigo-react';
import { AsyncButton } from '../../../components/AsyncButton';
import { Formik, Form, FormikHelpers } from 'formik';
import { MarkdownField } from './MarkdownField';

interface PostFormProps {
  initial: PostFormSchema;
  cancel?: boolean;
  history?: any;
  onSubmit: (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => Promise<any>;
  submitLabel: string;
  loadingText: string;
}

const formSchema = Yup.object({
  title: Yup.string().required('Title cannot be blank'),
  body: Yup.string().required('Post cannot be blank')
});

export interface PostFormSchema {
  title: string;
  body: string;
}

export function PostForm(props: PostFormProps) {
  const { initial, onSubmit, submitLabel, loadingText, cancel, history } = props;

  return (
    <Col width="100%" height="100%" p={[2, 4]}>
      <Formik
        validationSchema={formSchema}
        initialValues={initial}
        onSubmit={onSubmit}
        validateOnBlur
      >
        <Form style={{ display: 'contents' }}>
          <Row flexShrink='0' flexDirection={['column-reverse', 'row']} mb={4} gapX={4} justifyContent='space-between'>
            <Input maxWidth='40rem' width='100%' flexShrink={[0, 1]} placeholder="Post Title" id="title" />
              <Row flexDirection={['column', 'row']} mb={[4,0]}>
              <AsyncButton
                ml={[0,2]}
                flexShrink={0}
                primary
                loadingText={loadingText}
              >
                {submitLabel}
              </AsyncButton>
              {cancel && <Button
              ml={[0,2]}
              mt={[2,0]}
              onClick={() => {
                history.goBack();
              }}
              type="button"
                         >Cancel</Button>}
            </Row>
          </Row>
          <MarkdownField flexGrow={1} id="body" />
        </Form>
      </Formik>
    </Col>
  );
}
