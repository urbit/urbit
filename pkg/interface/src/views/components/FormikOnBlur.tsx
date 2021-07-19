import { FormikConfig, FormikProvider, FormikValues, useFormik } from 'formik';
import React, { useEffect, useImperativeHandle, useCallback } from 'react';

export function FormikOnBlur<
  Values extends FormikValues = FormikValues,
  ExtraProps = {}
>(props: FormikConfig<Values> & ExtraProps) {
  const formikBag = useFormik<Values>({ ...props, validateOnBlur: true });

  const trySubmit = useCallback(_.debounce((formikBag) => {
    if (
      Object.keys(formikBag.errors || {}).length === 0 &&
      formikBag.dirty &&
      !formikBag.isSubmitting
    ) {
      formikBag.submitForm();
    }
  }, 100), []);

  useEffect(() => {
    trySubmit(formikBag);
  }, [
    formikBag.values,
    formikBag.errors
  ]);

  useEffect(() => {
    formikBag.resetForm({ values: props.initialValues });
  }, [props.initialValues]);

  const { children, innerRef } = props;

  useImperativeHandle(innerRef, () => formikBag);

  return <FormikProvider value={formikBag}>{children}</FormikProvider>;
}
