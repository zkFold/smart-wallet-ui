export interface UTxO {
    address: string;
    tx_hash: string;
    tx_index: number;
    output_index: number;
    amount: Asset[];
    block: string;
    data_hash: string | null;
    inline_datum: string | null;
    reference_script_hash: string | null;
}

export interface Asset {
    unit: string;
    quantity: number;
}
