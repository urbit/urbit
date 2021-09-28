import { Button, Row } from '@tlon/indigo-react';
import { useFormikContext } from 'formik';
import React, { ReactElement, ReactNode, useCallback } from 'react';
import { AsyncButton } from './AsyncButton';

interface FormSubmitProps {
  children?: ReactNode;
  start?: boolean;
}

export function FormSubmit<T = unknown>(props: FormSubmitProps): ReactElement {
  const { children } = props;
  const { initialValues, values, dirty, resetForm, isSubmitting } = useFormikContext<T>();

  const handleSuccess = useCallback(() => {
    resetForm({ errors: {}, touched: {}, values, status: {} });
  }, [resetForm, values]);

  const handleRevert = useCallback(() => {
    resetForm({ errors: {}, touched: {}, values: initialValues, status: {} });
  }, [resetForm, initialValues]);

  return (
    <Row
      p={2}
      bottom="0px"
      justifyContent={props.start ? 'flex-start' : 'flex-end'}
      gapX={2}
      alignItems="center"
    >
      {dirty && !isSubmitting && (
        <Button onClick={handleRevert} backgroundColor="washedGray">
          Cancel
        </Button>
      )}
      <AsyncButton disabled={!dirty} onSuccess={handleSuccess} primary>
        {children}
      </AsyncButton>
    </Row>
  );
}
