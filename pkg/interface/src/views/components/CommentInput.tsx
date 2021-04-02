import React from 'react';
import * as Yup from 'yup';
import { Formik, FormikHelpers, Form, useFormikContext } from 'formik';
import { AsyncButton } from './AsyncButton';
import { ManagedTextAreaField as TextArea } from '@tlon/indigo-react';

interface FormSchema {
  comment: string;
}

const formSchema = Yup.object({
  comment: Yup.string().required('Comment can\'t be empty')
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
const SubmitTextArea = (props) => {
  const { submitForm } = useFormikContext<FormSchema>();
  const onKeyDown = (e: KeyboardEvent) => {
    if ((e.getModifierState('Control') || e.metaKey) && e.key === 'Enter') {
      submitForm();
    }
  };
  return <TextArea onKeyDown={onKeyDown} {...props} />;
};

export default function CommentInput(props: CommentInputProps) {
  const initialValues: FormSchema = { comment: props.initial || '' };
  const label = props.label || 'Add Comment';
  const loading = props.loadingText || 'Commenting...';

  return (
    <Formik
      validationSchema={formSchema}
      onSubmit={props.onSubmit}
      initialValues={initialValues}
      validateOnBlur={false}
      validateOnChange={false}
    >
      <Form>
        <SubmitTextArea
          id="comment"
          placeholder={props.placeholder || ''}
        />
        <AsyncButton mt={2} loadingText={loading} border type="submit">
          {label}
        </AsyncButton>
      </Form>
    </Formik>
  );
}
