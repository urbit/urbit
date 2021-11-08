import {
  Action,
  BaseTextArea as TextArea,
  Box,
  Col,
  Row
} from '@tlon/indigo-react';
import {
  Formik,
  FormikHelpers,
  useField,
  useFormikContext
} from 'formik';
import React, { useEffect, useMemo } from 'react';
import * as Yup from 'yup';
import { ShipImage } from './ShipImage';

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
  label?: string;
  placeholder?: string;
}
const SubmitTextArea = (props) => {
  const { submitForm } = useFormikContext<FormSchema>();
  const [field] = useField(props.id);
  const onKeyDown = (e: KeyboardEvent) => {
    if ((e.getModifierState('Control') || e.metaKey) && e.key === 'Enter') {
      submitForm();
      e.preventDefault();
    }
  };
  return (
    <TextArea
      lineHeight="tall"
      backgroundColor="white"
      color="black"
      fontFamily="sans"
      fontWeight="500"
      fontSize="1"
      flexGrow={1}
      style={{ resize: 'vertical' }}
      {...field}
      onKeyDown={onKeyDown}
      {...props}
    />
  );
};

function FormikHelper(props: { initialValues: any }) {
  const { initialValues } = props;
  const { resetForm } = useFormikContext();

  useEffect(() => {
    resetForm(initialValues);
  }, [initialValues]);

  return null;
}
const emptyStr = '';

export default function CommentInput(props: CommentInputProps) {
  const initialValues: FormSchema = useMemo(
    () => ({ comment: props.initial || emptyStr }),
  [props.initial]);

  const label = props.label || 'Comment';

  return (
    <Row
      marginLeft="-8px"
      width="105%"
      border="1"
      borderColor="lightGray"
      borderRadius="2"
      flexShrink={0}
    >
      <Box p="2">
        <ShipImage ship={`~${window.ship}`} />
      </Box>
      <Formik
        validationSchema={formSchema}
        onSubmit={props.onSubmit}
        initialValues={initialValues}
        validateOnBlur={false}
        validateOnChange={false}
      >
       {({ submitForm }) => (
         <Col pb="1" pr="2" pt="2" flexGrow={1}>
          <FormikHelper initialValues={initialValues} />
          <SubmitTextArea
            width="100%"
            id="comment"
            placeholder={props.placeholder || ''}
          />
          <Action
            type="submit"
            my="1"
            width="fit-content"
            alignSelf="flex-end"
            backgroundColor="white"
            onClick={submitForm}
          >
            {label}
          </Action>
        </Col>
       )}
      </Formik>
    </Row>
  );
}
