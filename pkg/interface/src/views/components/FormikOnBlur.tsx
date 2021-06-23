import { FormikConfig, FormikProvider, FormikValues, useFormik } from 'formik';
import React, { useEffect, useImperativeHandle, useState } from 'react';

export function FormikOnBlur<
  Values extends FormikValues = FormikValues,
  ExtraProps = {}
>(props: FormikConfig<Values> & ExtraProps) {
  const formikBag = useFormik<Values>({ ...props, validateOnBlur: true });
  const [submitting, setSubmitting] = useState(false);

  useEffect(() => {
    if (
      Object.keys(formikBag.errors || {}).length === 0 &&
      formikBag.dirty &&
      !formikBag.isSubmitting &&
      !submitting
    ) {
      setSubmitting(true);
      const { values } = formikBag;
      formikBag.submitForm().then(() => {
        formikBag.resetForm({ values });
        setSubmitting(false);
      });
    }
  }, [
    formikBag.errors,
    formikBag.dirty,
    submitting,
    formikBag.isSubmitting
  ]);

  useEffect(() => {
    formikBag.resetForm({ values: props.initialValues });
  }, [props.initialValues]);

  const { children, innerRef } = props;

  useImperativeHandle(innerRef, () => formikBag);

  return <FormikProvider value={formikBag}>{children}</FormikProvider>;
}
