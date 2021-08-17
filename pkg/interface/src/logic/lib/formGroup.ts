import React from "react";

export type SubmitHandler = () => Promise<any>;
interface IFormGroupContext {
  addSubmit: (id: string, submit: SubmitHandler) => void;
  onDirty: (id: string, touched: boolean) => void;
  onErrors: (id: string, errors: boolean) => void;
  submitAll: () => Promise<any>;
}

const fallback: IFormGroupContext = {
  addSubmit: () => {},
  onDirty: () => {},
  onErrors: () => {},
  submitAll: () => Promise.resolve(),
};

export const FormGroupContext = React.createContext(fallback);
