open import Data.Nat
    using (ℕ; zero; suc)
    renaming (_+_ to add)
open import Data.Vec
    using (Vec; []; _∷_)
open import Data.Product
    using (_×_)
    renaming (_,′_ to _,_) 
open import Data.Unit
    using (⊤) 

--------------------------------------------------------------------------------

Shape : ℕ → Set
Shape d = Vec ℕ d

Tensor : {d : ℕ} → Set → Shape d → Set
Tensor A [] = ⊤
Tensor A (m ∷ s) = Vec A m × Tensor A s

-- data Tensor Set : {n : ℕ} → Shape n → Set where
--     unit : {A : Set} → Tensor A []
--     _:::_ : {n m : ℕ} → {s : Shape m} → {A : Set} → Vec A n → Tensor A s → Tensor A s
    -- Tensor A (m ∷ s) = Vec A m × Tensor A s

-- _+_ : {n : ℕ} {A : Set} → {s : Shape n} → Tensor A s → Tensor A s → Tensor A s
-- x + y = {! x  !}

