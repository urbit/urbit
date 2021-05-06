import { FormikConfig, FormikProvider, FormikValues, useFormik } from 'formik';
import React, { useEffect, useImperativeHandle } from 'react';

export function FormikOnBlur<
  Values extends FormikValues = FormikValues,
  ExtraProps = {}
>(props: FormikConfig<Values> & ExtraProps) {
  const formikBag = useFormik<Values>({ ...props, validateOnBlur: true });

  useEffect(() => {
    if (
      Object.keys(formikBag.errors || {}).length === 0 &&
      Object.keys(formikBag.touched || {}).length !== 0 &&
      !formikBag.isSubmitting
    ) {
      const { values } = formikBag;
      formikBag.submitForm().then(() => {
        formikBag.resetForm({ values, touched: {} });
      });
    }
  }, [
    formikBag.errors,
    formikBag.touched,
    formikBag.submitForm,
    formikBag.values
  ]);

  const { children, innerRef } = props;

  useImperativeHandle(innerRef, () => formikBag);

  return <FormikProvider value={formikBag}>{children}</FormikProvider>;
}
