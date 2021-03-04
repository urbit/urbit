import React, { useCallback, ReactNode, ReactElement } from 'react';
import { useFormikContext } from 'formik';
import { Row, Button } from '@tlon/indigo-react';
import { AsyncButton } from './AsyncButton';

interface FormSubmitProps {
  children?: ReactNode;
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
      p="2"
      bottom="0px"
      justifyContent="flex-end"
      gapX="2"
      alignItems="center"
    >
      {dirty && !isSubmitting && (
        <Button onClick={handleRevert} backgroundColor="washedGray">
          Cancel
        </Button>
      )}
      <AsyncButton onSuccess={handleSuccess} primary>
        {children}
      </AsyncButton>
    </Row>
  );
}
