import React, {
  ReactNode,
  useEffect,
  useCallback,
  useState,
  useMemo,
} from "react";
import { Button, Box, Row, Col } from "@tlon/indigo-react";
import _ from "lodash";
import { useFormikContext } from "formik";
import { PropFunc } from "~/types";
import { FormGroupContext, SubmitHandler } from "~/logic/lib/formGroup";
import { StatelessAsyncButton } from "./StatelessAsyncButton";

export function useFormGroupContext(id: string) {
  const ctx = React.useContext(FormGroupContext);
  const addSubmit = useCallback(
    (submit: SubmitHandler) => {
      ctx.addSubmit(id, submit);
    },
    [ctx.addSubmit, id]
  );
  const onDirty = useCallback(
    (dirty: boolean) => {
      console.log(id, dirty);
      ctx.onDirty(id, dirty);
    },
    [ctx.onDirty, id]
  );

  const onErrors = useCallback(
    (errors: boolean) => {
      ctx.onErrors(id, errors);
    },
    [ctx.onErrors, id]
  );

  return {
    onDirty,
    addSubmit,
    onErrors,
  };
}

export function FormGroupChild(props: { id: string }) {
  const { id } = props;
  const { addSubmit, onDirty, onErrors } = useFormGroupContext(id);
  const { submitForm, dirty, errors } = useFormikContext();

  useEffect(() => {
    addSubmit(submitForm);
  }, [submitForm]);

  useEffect(() => {
    onDirty(dirty);
  }, [dirty, onDirty]);

  useEffect(() => {
    onErrors(_.keys(_.pickBy(errors, (s) => !!s)).length > 0);
  }, [errors, onErrors]);

  return <Box display="none" />;
}

export function FormGroup(props: PropFunc<typeof Box>) {
  const { children, ...rest } = props;
  const [submits, setSubmits] = useState({} as { [id: string]: SubmitHandler });
  const [dirty, setDirty] = useState({} as Record<string, boolean>);
  const [errors, setErrors] = useState({} as Record<string, boolean>);
  const addSubmit = useCallback((id: string, s: SubmitHandler) => {
    setSubmits((ss) => ({ ...ss, [id]: s }));
  }, []);

  const submitAll = useCallback(async () => {
    await Promise.all(
      _.map(
        _.pickBy(submits, (_v, k) => dirty[k]),
        (f) => f()
      )
    );
  }, [submits, dirty]);

  const onDirty = useCallback(
    (id: string, t: boolean) => {
      setDirty((ts) => ({ ...ts, [id]: t }));
    },
    [setDirty]
  );

  const onErrors = useCallback((id: string, e: boolean) => {
    setErrors((es) => ({ ...es, [id]: e }));
  }, []);

  const context = { addSubmit, submitAll, onErrors, onDirty };

  const hasErrors = useMemo(
    () => _.keys(_.pickBy(errors, (s) => !!s)).length > 0,
    [errors]
  );
  const isDirty = useMemo(
    () => _.keys(_.pickBy(dirty, _.identity)).length > 0,
    [dirty]
  );

  return (
    <Box {...rest} position="relative">
      <FormGroupContext.Provider value={context}>
        {children}
      </FormGroupContext.Provider>
      {isDirty && (
        <Row
          justifyContent="flex-end"
          width="100%"
          position="sticky"
          bottom="0px"
          p="3"
          gapX="2"
          backgroundColor="white"
          borderTop="1"
          borderTopColor="washedGray"
        >
          <Button>Cancel</Button>
          <StatelessAsyncButton
            onClick={submitAll}
            disabled={hasErrors}
            primary
          >
            Save Changes
          </StatelessAsyncButton>
        </Row>
      )}
    </Box>
  );
}
